(ns anthropic.organization
  "Clojure wrappers over the GA Organization API and beta-only administration APIs of the
  official Anthropic Java SDK: the organization itself plus users, API keys,
  external keys, invites, rate limits, service accounts, workspaces, workspace
  members/rate-limits/service-accounts, and identity federation (issuers, rules,
  and rule workspaces).

  GA operations use the stable client path; beta-only administration operations
  remain under the beta client. Build a request as a
  Clojure map, get a Clojure map back. Errors follow `anthropic.core`'s
  contract: API/IO failures are ex-info keyed `:anthropic/error` with the SDK
  exception as cause.

  Deeply nested response sub-objects (an API key's principal/scope, an external
  key's provider config/attachment, a federation issuer's JWKS, a rule's
  match/target/attributes) are returned as full-fidelity Clojure data. For the
  matching request inputs, scalar fields are accepted as idiomatic maps; the
  richest union inputs (inline JWKS keys, Azure external-key configs, match
  claims) also accept a ready SDK object as an escape hatch."
  (:require [anthropic.core]
            [clojure.string :as str])
  (:import (com.anthropic.client AnthropicClient)
           (com.anthropic.core JsonValue)
           (com.anthropic.errors AnthropicException)
           (com.anthropic.models.organization.compliancesettings
                                             OrganizationComplianceSettings
                                             ComplianceSettingUpdateParams)
           (java.util Optional)))

(set! *warn-on-reflection* true)

;; ---- Shared helpers -------------------------------------------------------

(def ^:private throw-normalized! @#'anthropic.core/throw-normalized!)
(def ^:private json->clj @#'anthropic.core/json->clj)

(defmacro ^:private with-api-errors [& body]
  `(try ~@body
        (catch AnthropicException e# (throw-normalized! e#))))

(defn- missing-key! [k]
  (throw (ex-info (str "Missing required key " k)
                  {:anthropic/error :missing-key :key k})))

(defn- ->keyword [x]
  (-> x str str/lower-case (str/replace "_" "-") keyword))

(defn- unopt [^Optional o]
  (when (and o (.isPresent o)) (.get o)))

(declare enum-keywords kw<-)

(defn- enum-keywords
  "Replace SDK enum values with idiomatic keywords while retaining the SDK's
  JSON-shaped maps.  Generated model classes expose their fields as accessors,
  so walking those accessors lets us distinguish an enum from an ordinary
  string such as an id or a display name."
  [o value]
  (cond
    (instance? com.anthropic.core.Enum o)
    (kw<- (clojure.lang.Reflector/invokeInstanceMethod o "asString" (object-array 0)))
    (instance? Optional o) (when (.isPresent ^Optional o)
                            (enum-keywords (.get ^Optional o) value))
    (instance? java.util.List o) (mapv enum-keywords o value)
    (and (some? o) (map? value)
         (.startsWith (.getName (class o)) "com.anthropic.models."))
    (reduce (fn [m ^java.lang.reflect.Method method]
              (let [name (.getName method)
                    key (-> name
                            (str/replace #"([a-z])([A-Z])" "$1-$2")
                            str/lower-case
                            keyword)]
                (if (and (zero? (alength (.getParameterTypes method)))
                         (contains? m key)
                         (not (str/starts-with? name "_")))
                  (assoc m key (enum-keywords (.invoke method o (object-array 0))
                                               (get m key)))
                  m)))
            value (.getMethods (class o)))
    :else value))

(defn- kebab-keys [value]
  (cond
    (map? value) (into {}
                        (map (fn [[k v]]
                               [(-> k name (str/replace "_" "-") keyword)
                                (kebab-keys v)]))
                        value)
    (sequential? value) (mapv kebab-keys value)
    :else value))

(defn- obj->clj
  "Full-fidelity Clojure data for any SDK model object, via its JSON form."
  [^Object o]
  (when (some? o)
    (enum-keywords o (kebab-keys (json->clj (JsonValue/from o))))))

(defn- kw<-
  "Wire enum string -> kebab keyword. Call sites type-hint the concrete enum
  before `.asString`, since the string accessor lives on each SDK enum type."
  [s]
  (when (some? s) (->keyword s)))

(defn- ->wire
  "Kebab keyword/string -> the SDK's lower_snake wire string."
  [value]
  (-> value name str/lower-case (str/replace "-" "_")))

(defn- check-enum! [value allowed k]
  (let [kw (if (keyword? value) value (->keyword value))]
    (when-not (contains? allowed kw)
      (throw (ex-info (str "Unknown " (name k) " " value)
                      {:anthropic/error :invalid-enum-value :key k :value value})))
    kw))

(def ^:private invite-roles #{:billing :claude-code-user :developer :managed :user})
(def ^:private user-roles #{:billing :claude-code-user :developer :managed :user})
(def ^:private sa-org-roles #{:admin :developer})
(def ^:private ws-roles
  #{:workspace-admin :workspace-billing :workspace-developer
    :workspace-restricted-developer :workspace-user})
(def ^:private no-billing-ws-roles
  #{:workspace-admin :workspace-developer :workspace-restricted-developer :workspace-user})
(def ^:private api-key-statuses #{:active :archived :expired :inactive})
(def ^:private api-key-update-statuses #{:active :archived :inactive})
(def ^:private invite-list-statuses #{:accepted :expired :pending})
(def ^:private group-types #{:batch :files :model-group :skills :token-count :web-search})
(def ^:private geos #{:us})

;; The beta-only administration APIs share a large, rapidly growing family of
;; generated parameter objects. These helpers preserve the library's map-in /
;; map-out contract while delegating field validation to the SDK builders.
(defn- dynamic-call [target method & args]
  (clojure.lang.Reflector/invokeInstanceMethod target method (to-array args)))

(defn- accepts-args?
  "True when every arg is an instance of the matching (boxed) parameter type."
  [^java.lang.reflect.Method m args]
  (every? (fn [[^Class t a]]
            (cond
              (nil? a) (not (.isPrimitive t))
              (= t Boolean/TYPE) (boolean? a)
              (.isPrimitive t) (number? a)
              :else (instance? t a)))
          (map vector (.getParameterTypes m) args)))

(defn- dynamic-static-call
  "Invoke the static `method` on `target` whose parameter types accept `args`.
  `getMethods` order is unspecified, so overloads (e.g. several one-arg `of`)
  must be chosen by type, not position."
  [^Class target method & args]
  (let [arity-matches (filter #(and (= method (.getName ^java.lang.reflect.Method %))
                                    (= (count args) (alength (.getParameterTypes ^java.lang.reflect.Method %))))
                              (.getMethods target))
        ^java.lang.reflect.Method m (or (first (filter #(accepts-args? % args) arity-matches))
                                        (first arity-matches))]
    (.invoke m nil (object-array args))))

(defn- camel-key [k]
  (let [[head & tail] (str/split (name k) #"-")]
    (apply str head (map str/capitalize tail))))

(defn- type-class [^java.lang.reflect.Type type]
  (cond
    (instance? Class type) type
    (instance? java.lang.reflect.ParameterizedType type)
    (let [raw (.getRawType ^java.lang.reflect.ParameterizedType type)]
      (when (instance? Class raw) raw))))

(defn- sdk-enum
  "Resolve a keyword or string to an instance of the SDK enum class `target`.
  Known constants match either their wire value or their Java constant name
  (so `:0-200k` and `:from-0-to-200k` both resolve to `FROM_0_TO_200K`, whose
  wire value is `0-200k`). Unknown values fall back to `of` with the
  lower_snake wire form, preserving forward compatibility."
  [^Class target value]
  (let [wanted (->keyword (name value))
        known (for [^java.lang.reflect.Field f (.getFields target)
                    :when (and (java.lang.reflect.Modifier/isStatic (.getModifiers f))
                               (= target (.getType f)))]
                [f (.get f nil)])]
    (or (some (fn [[^java.lang.reflect.Field f e]]
                (when (or (= wanted (->keyword (.getName f)))
                          (= wanted (->keyword (str (dynamic-call e "asString")))))
                  e))
              known)
        (dynamic-static-call target "of" (->wire value)))))

(defn- dynamic-value [^Class target generic-type value]
  (cond
    (nil? value) nil
    (and (.isAssignableFrom com.anthropic.core.Enum target)
         (or (keyword? value) (string? value)))
    (sdk-enum target value)
    (= target String) (str value)
    (or (= target Long/TYPE) (= target Long)) (long value)
    (or (= target Integer/TYPE) (= target Integer)) (int value)
    (or (= target Boolean/TYPE) (= target Boolean)) (boolean value)
    (= target java.time.LocalDate) (if (instance? java.time.LocalDate value)
                                      value (java.time.LocalDate/parse (str value)))
    (= target java.time.OffsetDateTime) (if (instance? java.time.OffsetDateTime value)
                                           value (java.time.OffsetDateTime/parse (str value)))
    (.isAssignableFrom java.util.List target)
    (let [element-type (when (instance? java.lang.reflect.ParameterizedType generic-type)
                         (type-class (first (.getActualTypeArguments
                                             ^java.lang.reflect.ParameterizedType generic-type))))]
      (mapv #(dynamic-value (or element-type Object) nil %) value))
    ;; Non-enum SDK value types: keywords go to the wire form, but strings
    ;; (ids, URLs, free text) are handed over untouched.
    (or (keyword? value) (string? value))
    (try (dynamic-static-call target "of" (if (keyword? value) (->wire value) value))
         (catch Throwable _ value))
    :else value))

(def ^:private wrapper-param-types
  #{java.util.Optional com.anthropic.core.JsonField JsonValue com.anthropic.core.MultipartField})

(defn- builder-setter
  "Pick the builder overload for `method-name` that should receive `value`.
  Optional overloads are never used. Plain-typed overloads beat the raw
  JsonField/JsonValue/MultipartField ones (which would otherwise receive an
  unconverted or wire-mangled value), and among plain overloads one that
  already accepts `value` as-is (e.g. InputStream vs byte[] vs Path) wins."
  ^java.lang.reflect.Method [builder method-name value]
  (let [param-type (fn [^java.lang.reflect.Method m] (aget (.getParameterTypes m) 0))
        candidates (filter #(and (= method-name (.getName ^java.lang.reflect.Method %))
                                 (= 1 (alength (.getParameterTypes ^java.lang.reflect.Method %)))
                                 (not= java.util.Optional (param-type %)))
                           (.getMethods (class builder)))
        plain (remove #(contains? wrapper-param-types (param-type %)) candidates)]
    (or (first (filter #(instance? (param-type %) value) plain))
        (first plain)
        (first candidates))))

(defn- ->dynamic-params [class-name opts]
  (let [^Class param-class (Class/forName class-name)
        builder (dynamic-static-call param-class "builder")]
    (doseq [[key value] opts :when (some? value)]
      (let [method (builder-setter builder (camel-key key) value)]
        (when-not method
          (throw (ex-info (str "Unknown option " key " for " class-name)
                          {:anthropic/error :invalid-option :key key})))
        (try
          (.invoke method builder
                   (object-array
                    [(dynamic-value (aget (.getParameterTypes method) 0)
                                    (aget (.getGenericParameterTypes method) 0)
                                    value)]))
          (catch java.lang.reflect.InvocationTargetException e
            (throw (or (.getCause e) e))))))
    (dynamic-call builder "build")))

(defn- beta-organization [^AnthropicClient client]
  (-> (.beta client) (.organization)))

(defn- dynamic-list [service params]
  (let [page (dynamic-call service "list" params)
        pager (dynamic-call page "autoPager")]
    (mapv obj->clj (iterator-seq (dynamic-call pager "iterator")))))

;; ---- Organization ---------------------------------------------------------

(defn- organization->map [r]
  (let [m (obj->clj r)]
    {:id (:id m) :name (:name m)}))

(defn get-organization
  "Retrieve the caller's organization, as `{:id ... :name ...}`."
  [^AnthropicClient client]
  (with-api-errors
    (organization->map (-> (.organization client) (.retrieve)))))

;; ---- Compliance settings --------------------------------------------------

(defn- ->compliance-state [state]
  (case (check-enum! state #{:enabled :disabled} :state)
    :enabled (com.anthropic.models.organization.compliancesettings.ComplianceSettingsStateParam/ofEnabled
              (.build (com.anthropic.models.organization.compliancesettings.ComplianceSettingsStateEnabledParam/builder)))
    :disabled (com.anthropic.models.organization.compliancesettings.ComplianceSettingsStateParam/ofDisabled
               (.build (com.anthropic.models.organization.compliancesettings.ComplianceSettingsStateDisabledParam/builder)))))

(defn- ->compliance-update-params ^ComplianceSettingUpdateParams [{:keys [state]}]
  (when-not state (missing-key! :state))
  (let [b (ComplianceSettingUpdateParams/builder)
        ^com.anthropic.models.organization.compliancesettings.ComplianceSettingsStateParam state-value
        (->compliance-state state)]
    (.state b state-value)
    (.build b)))

(defn- compliance-settings->map [^OrganizationComplianceSettings settings]
  {:state (kw<- (.asString (.type (.state settings))))})

(defn get-compliance-settings
  "Retrieve organization compliance settings as `{:state :enabled|:disabled}`."
  [^AnthropicClient client]
  (with-api-errors
    (compliance-settings->map (-> (.organization client)
                                  (.complianceSettings) (.retrieve)))))

(defn update-compliance-settings
  "Update organization compliance settings with `{:state :enabled|:disabled}`."
  [^AnthropicClient client changes]
  (with-api-errors
    (compliance-settings->map (-> (.organization client)
                                  (.complianceSettings)
                                  (.update (->compliance-update-params changes))))))

;; ---- Users ----------------------------------------------------------------

(defn- org-user->map [^com.anthropic.models.organization.users.OrganizationUser r]
  {:id (.id r)
   :added-at (str (.addedAt r))
   :email (.email r)
   :name (.name r)
   :role (kw<- (.asString ^com.anthropic.models.organization.OrganizationRole (.role r)))})

(defn get-org-user
  "Retrieve one organization user by id."
  [^AnthropicClient client ^String user-id]
  (with-api-errors
    (org-user->map (-> (.organization client) (.users) (.retrieve user-id)))))

(defn update-org-user
  "Update an organization user's `:role` (one of the organization roles)."
  [^AnthropicClient client ^String user-id {:keys [role]}]
  (with-api-errors
    (let [b (com.anthropic.models.organization.users.UserUpdateParams/builder)]
      (.userId b ^String user-id)
      (when role
        (.role b (com.anthropic.models.organization.users.UserUpdateParams$Role/of
                  (->wire (check-enum! role user-roles :role)))))
      (org-user->map (-> (.organization client) (.users) (.update (.build b)))))))

(defn list-org-users
  "List organization users. Options: `:limit`, `:after-id`, `:before-id`,
  `:email`, `:roles` (a seq of role strings)."
  ([^AnthropicClient client] (list-org-users client {}))
  ([^AnthropicClient client {:keys [limit after-id before-id email roles]}]
   (with-api-errors
     (let [b (com.anthropic.models.organization.users.UserListParams/builder)]
       (when limit (.limit b (long limit)))
       (when after-id (.afterId b ^String after-id))
       (when before-id (.beforeId b ^String before-id))
       (when email (.email b ^String email))
       (when (seq roles) (.roles b ^java.util.List (mapv str roles)))
       (mapv org-user->map (.autoPager (-> (.organization client) (.users)
                                           (.list (.build b)))))))))

(defn remove-org-user
  "Remove a user from the organization. Returns the removal response as a map."
  [^AnthropicClient client ^String user-id]
  (with-api-errors
    (obj->clj (-> (.organization client) (.users) (.remove user-id)))))

;; ---- API keys -------------------------------------------------------------

(defn- api-key->map [^com.anthropic.models.beta.organization.apikeys.BetaApiKey r]
  {:id (.id r)
   :created-at (str (.createdAt r))
   :created-by (obj->clj (unopt (.createdBy r)))
   :expires-at (some-> (.expiresAt r) unopt str)
   :name (.name r)
   :partial-key-hint (unopt (.partialKeyHint r))
   :principal (obj->clj (unopt (.principal r)))
   :scope (obj->clj (.scope r))
   :status (kw<- (.asString ^com.anthropic.models.beta.organization.apikeys.BetaApiKey$Status (.status r)))
   :workspace-id (unopt (.workspaceId r))})

(defn get-api-key
  "Retrieve one organization API key by id."
  [^AnthropicClient client ^String api-key-id]
  (with-api-errors
    (let [b (com.anthropic.models.beta.organization.apikeys.ApiKeyRetrieveParams/builder)]
      (.apiKeyId b ^String api-key-id)
      (api-key->map (-> (.beta client) (.organization) (.apiKeys) (.retrieve (.build b)))))))

(defn update-api-key
  "Update an API key's `:name` and/or `:status` (`:active`, `:archived`,
  `:inactive`)."
  [^AnthropicClient client ^String api-key-id {:keys [name status]}]
  (with-api-errors
    (let [b (com.anthropic.models.beta.organization.apikeys.ApiKeyUpdateParams/builder)]
      (.apiKeyId b ^String api-key-id)
      (when name (.name b ^String name))
      (when status
        (.status b (com.anthropic.models.beta.organization.apikeys.ApiKeyUpdateParams$Status/of
                    (->wire (check-enum! status api-key-update-statuses :status)))))
      (api-key->map (-> (.beta client) (.organization) (.apiKeys) (.update (.build b)))))))

(defn list-api-keys
  "List organization API keys. Options: `:limit`, `:after-id`, `:before-id`,
  `:status` (`:active`/`:archived`/`:expired`/`:inactive`), `:workspace-id`,
  `:created-by-user-id`."
  ([^AnthropicClient client] (list-api-keys client {}))
  ([^AnthropicClient client {:keys [limit after-id before-id status workspace-id created-by-user-id]}]
   (with-api-errors
     (let [b (com.anthropic.models.beta.organization.apikeys.ApiKeyListParams/builder)]
       (when limit (.limit b (long limit)))
       (when after-id (.afterId b ^String after-id))
       (when before-id (.beforeId b ^String before-id))
       (when workspace-id (.workspaceId b ^String workspace-id))
       (when created-by-user-id (.createdByUserId b ^String created-by-user-id))
       (when status
         (.status b (com.anthropic.models.beta.organization.apikeys.ApiKeyListParams$Status/of
                     (->wire (check-enum! status api-key-statuses :status)))))
       (mapv api-key->map (.autoPager (-> (.beta client) (.organization) (.apiKeys)
                                          (.list (.build b)))))))))

;; ---- External keys --------------------------------------------------------

(defn- external-key->map [^com.anthropic.models.organization.externalkeys.ExternalKey r]
  {:id (.id r)
   :attachment (obj->clj (.attachment r))
   :created-at (str (.createdAt r))
   :display-name (unopt (.displayName r))
   :geo (.geo r)
   :provider-config (obj->clj (.providerConfig r))
   :updated-at (str (.updatedAt r))})

(defn create-external-key
  "Create an external key. Provide a provider config as one of
  `:aws-provider-config`/`:gcp-provider-config` (a key name string) or
  `:provider-config` (a ready SDK config object). Options: `:display-name`,
  `:geo` (`:us`)."
  [^AnthropicClient client {:keys [display-name geo aws-provider-config gcp-provider-config provider-config]}]
  (with-api-errors
    (let [b (com.anthropic.models.organization.externalkeys.ExternalKeyCreateParams/builder)]
      (cond
        aws-provider-config (.awsProviderConfig b ^String aws-provider-config)
        gcp-provider-config (.gcpProviderConfig b ^String gcp-provider-config)
        provider-config (.providerConfig b ^com.anthropic.models.organization.externalkeys.ExternalKeyCreateParams$ProviderConfig provider-config))
      (when display-name (.displayName b ^String display-name))
      (when geo
        (.geo b (com.anthropic.models.organization.externalkeys.ExternalKeyCreateParams$Geo/of
                 (->wire (check-enum! geo geos :geo)))))
      (external-key->map (-> (.organization client) (.externalKeys)
                             (.create (.build b)))))))

(defn get-external-key
  "Retrieve one external key by id."
  [^AnthropicClient client ^String external-key-id]
  (with-api-errors
    (let [b (com.anthropic.models.organization.externalkeys.ExternalKeyRetrieveParams/builder)]
      (.externalKeyId b ^String external-key-id)
      (external-key->map (-> (.organization client) (.externalKeys)
                             (.retrieve (.build b)))))))

(defn update-external-key
  "Update an external key's `:display-name`, provider config
  (`:aws-provider-config`/`:gcp-provider-config`/`:provider-config`), or `:geo`."
  [^AnthropicClient client ^String external-key-id
   {:keys [display-name geo aws-provider-config gcp-provider-config provider-config]}]
  (with-api-errors
    (let [b (com.anthropic.models.organization.externalkeys.ExternalKeyUpdateParams/builder)]
      (.externalKeyId b ^String external-key-id)
      (cond
        aws-provider-config (.awsProviderConfig b ^String aws-provider-config)
        gcp-provider-config (.gcpProviderConfig b ^String gcp-provider-config)
        provider-config (.providerConfig b ^com.anthropic.models.organization.externalkeys.ExternalKeyUpdateParams$ProviderConfig provider-config))
      (when display-name (.displayName b ^String display-name))
      (when geo
        (.geo b (com.anthropic.models.organization.externalkeys.ExternalKeyUpdateParams$Geo/of
                 (->wire (check-enum! geo geos :geo)))))
      (external-key->map (-> (.organization client) (.externalKeys)
                             (.update (.build b)))))))

(defn list-external-keys
  "List external keys. Options: `:limit`, `:page`."
  ([^AnthropicClient client] (list-external-keys client {}))
  ([^AnthropicClient client {:keys [limit page]}]
   (with-api-errors
     (let [b (com.anthropic.models.organization.externalkeys.ExternalKeyListParams/builder)]
       (when limit (.limit b (long limit)))
       (when page (.page b ^String page))
       (mapv external-key->map (.autoPager (-> (.organization client) (.externalKeys)
                                               (.list (.build b)))))))))

(defn delete-external-key
  "Delete an external key by id. Returns the delete response as a map."
  [^AnthropicClient client ^String external-key-id]
  (with-api-errors
    (let [b (com.anthropic.models.organization.externalkeys.ExternalKeyDeleteParams/builder)]
      (.externalKeyId b ^String external-key-id)
      (obj->clj (-> (.organization client) (.externalKeys) (.delete (.build b)))))))

(defn validate-external-key
  "Validate an external key by id. Returns the validation response as a map."
  [^AnthropicClient client ^String external-key-id]
  (with-api-errors
    (let [b (com.anthropic.models.organization.externalkeys.ExternalKeyValidateParams/builder)]
      (.externalKeyId b ^String external-key-id)
      (obj->clj (-> (.organization client) (.externalKeys) (.validate (.build b)))))))

;; ---- Invites --------------------------------------------------------------

(defn- invite->map [^com.anthropic.models.organization.invites.OrganizationInvite r]
  {:id (.id r)
   :accepted-at (some-> (.acceptedAt r) unopt str)
   :email (.email r)
   :expires-at (str (.expiresAt r))
   :invited-at (str (.invitedAt r))
   :rbac-group-ids (vec (.rbacGroupIds r))
   :role (kw<- (.asString ^com.anthropic.models.organization.OrganizationRole (.role r)))
   :status (kw<- (.asString ^com.anthropic.models.organization.invites.OrganizationInvite$Status (.status r)))})

(defn create-invite
  "Create an organization invite. Requires `:email` and `:role` (one of the
  invite roles). Optional `:rbac-group-ids` (a seq of strings)."
  [^AnthropicClient client {:keys [email role rbac-group-ids]}]
  (with-api-errors
    (when-not email (missing-key! :email))
    (when-not role (missing-key! :role))
    (let [b (com.anthropic.models.organization.invites.InviteCreateParams/builder)]
      (.email b ^String email)
      (.role b (com.anthropic.models.organization.invites.InviteCreateParams$Role/of
                (->wire (check-enum! role invite-roles :role))))
      (when (seq rbac-group-ids) (.rbacGroupIds b ^java.util.List (mapv str rbac-group-ids)))
      (invite->map (-> (.organization client) (.invites) (.create (.build b)))))))

(defn get-invite
  "Retrieve one invite by id."
  [^AnthropicClient client ^String invite-id]
  (with-api-errors
    (let [b (com.anthropic.models.organization.invites.InviteRetrieveParams/builder)]
      (.inviteId b ^String invite-id)
      (invite->map (-> (.organization client) (.invites) (.retrieve (.build b)))))))

(defn list-invites
  "List invites. Options: `:limit`, `:after-id`, `:before-id`, `:email`,
  `:roles` (seq of role strings), `:statuses` (seq of `:accepted`/`:expired`/
  `:pending`)."
  ([^AnthropicClient client] (list-invites client {}))
  ([^AnthropicClient client {:keys [limit after-id before-id email roles statuses]}]
   (with-api-errors
     (let [b (com.anthropic.models.organization.invites.InviteListParams/builder)]
       (when limit (.limit b (long limit)))
       (when after-id (.afterId b ^String after-id))
       (when before-id (.beforeId b ^String before-id))
       (when email (.email b ^String email))
       (when (seq roles) (.roles b ^java.util.List (mapv str roles)))
       (when (seq statuses)
         (.statuses b ^java.util.List
                    (mapv #(com.anthropic.models.organization.invites.InviteListParams$Status/of
                            (->wire (check-enum! % invite-list-statuses :status)))
                          statuses)))
       (mapv invite->map (.autoPager (-> (.organization client) (.invites)
                                         (.list (.build b)))))))))

(defn delete-invite
  "Delete an invite by id. Returns the delete response as a map."
  [^AnthropicClient client ^String invite-id]
  (with-api-errors
    (let [b (com.anthropic.models.organization.invites.InviteDeleteParams/builder)]
      (.inviteId b ^String invite-id)
      (obj->clj (-> (.organization client) (.invites) (.delete (.build b)))))))

;; ---- Rate limits (organization) -------------------------------------------

(defn- ->rate-limit-group
  "Convert OrganizationRateLimit$Group union to Clojure map."
  [^com.anthropic.models.organization.ratelimits.OrganizationRateLimit$Group group]
  (when group
    (let [group-type (kw<- (.asString (.type group)))]
      (cond-> {:id (.id group) :type (name group-type)}
        (.isModel group)
        (assoc :display-name (.displayName ^com.anthropic.models.organization.ratelimits.OrganizationRateLimitModelGroup (.asModel group)))))))


(defn- rate-limit->map
  [^com.anthropic.models.organization.ratelimits.OrganizationRateLimit r]
  (cond-> {:id (.id r)
           :group (->rate-limit-group (.group r))
           :limits (mapv (fn [^com.anthropic.models.organization.ratelimits.OrganizationRateLimitValue limit]
                           {:type (.type limit)
                            :value (.value limit)})
                        (.limits r))}
    (some? (unopt (.models r)))
    (assoc :models (vec (unopt (.models r))))))


(defn list-rate-limits
  "List organization rate limits. Options: `:limit`, `:page`, `:model`,
  `:group-type` (`:batch`/`:files`/`:model-group`/`:skills`/`:token-count`/
  `:web-search`)."
  ([^AnthropicClient client] (list-rate-limits client {}))
  ([^AnthropicClient client {:keys [limit page model group-type]}]
   (with-api-errors
     (let [b (com.anthropic.models.organization.ratelimits.RateLimitListParams/builder)]
       (when limit (.limit b (long limit)))
       (when page (.page b ^String page))
       (when model (.model b ^String model))
       (when group-type
         (.groupType b (com.anthropic.models.organization.ratelimits.RateLimitListParams$GroupType/of
                        (->wire (check-enum! group-type group-types :group-type)))))
       (mapv rate-limit->map (.autoPager (-> (.organization client) (.rateLimits)
                                      (.list (.build b)))))))))

;; ---- Service accounts -----------------------------------------------------

(defn- service-account->map
  [^com.anthropic.models.organization.serviceaccounts.ServiceAccount r]
  {:id (.id r)
   :archived-at (some-> (.archivedAt r) unopt str)
   :archived-by-actor-id (unopt (.archivedByActorId r))
   :created-at (str (.createdAt r))
   :created-by-actor-id (unopt (.createdByActorId r))
   :description (unopt (.description r))
   :name (.name r)
   :organization-role (kw<- (.asString ^com.anthropic.models.organization.serviceaccounts.ServiceAccount$OrganizationRole (.organizationRole r)))
   :updated-at (str (.updatedAt r))
   :updated-by-actor-id (unopt (.updatedByActorId r))})

(defn create-service-account
  "Create a service account. Requires `:name`. Optional `:description`,
  `:organization-role` (`:admin`/`:developer`)."
  [^AnthropicClient client {:keys [name description organization-role]}]
  (with-api-errors
    (when-not name (missing-key! :name))
    (let [b (com.anthropic.models.organization.serviceaccounts.ServiceAccountCreateParams/builder)]
      (.name b ^String name)
      (when description (.description b ^String description))
      (when organization-role
        (.organizationRole b (com.anthropic.models.organization.serviceaccounts.ServiceAccountCreateParams$OrganizationRole/of
                              (->wire (check-enum! organization-role sa-org-roles :organization-role)))))
      (service-account->map (-> (.organization client) (.serviceAccounts)
                                (.create (.build b)))))))

(defn get-service-account
  "Retrieve one service account by id."
  [^AnthropicClient client ^String service-account-id]
  (with-api-errors
    (let [b (com.anthropic.models.organization.serviceaccounts.ServiceAccountRetrieveParams/builder)]
      (.serviceAccountId b ^String service-account-id)
      (service-account->map (-> (.organization client) (.serviceAccounts)
                                (.retrieve (.build b)))))))

(defn update-service-account
  "Update a service account's `:description` and/or `:organization-role`."
  [^AnthropicClient client ^String service-account-id {:keys [description organization-role]}]
  (with-api-errors
    (let [b (com.anthropic.models.organization.serviceaccounts.ServiceAccountUpdateParams/builder)]
      (.serviceAccountId b ^String service-account-id)
      (when description (.description b ^String description))
      (when organization-role
        (.organizationRole b (com.anthropic.models.organization.serviceaccounts.ServiceAccountUpdateParams$OrganizationRole/of
                              (->wire (check-enum! organization-role sa-org-roles :organization-role)))))
      (service-account->map (-> (.organization client) (.serviceAccounts)
                                (.update (.build b)))))))

(defn list-service-accounts
  "List service accounts. Options: `:limit`, `:page`, `:include-archived`."
  ([^AnthropicClient client] (list-service-accounts client {}))
  ([^AnthropicClient client {:keys [limit page include-archived]}]
   (with-api-errors
     (let [b (com.anthropic.models.organization.serviceaccounts.ServiceAccountListParams/builder)]
       (when limit (.limit b (long limit)))
       (when page (.page b ^String page))
       (when (some? include-archived) (.includeArchived b (boolean include-archived)))
       (mapv service-account->map (.autoPager (-> (.organization client) (.serviceAccounts)
                                                  (.list (.build b)))))))))

(defn archive-service-account
  "Archive a service account by id. Returns the archived service account."
  [^AnthropicClient client ^String service-account-id]
  (with-api-errors
    (let [b (com.anthropic.models.organization.serviceaccounts.ServiceAccountArchiveParams/builder)]
      (.serviceAccountId b ^String service-account-id)
      (service-account->map (-> (.organization client) (.serviceAccounts)
                                (.archive (.build b)))))))

;; ---- Service-account workspaces -------------------------------------------

(defn- sa-workspace-member->map
  [^com.anthropic.models.organization.serviceaccounts.ServiceAccountWorkspaceMember r]
  {:service-account-id (.serviceAccountId r)
   :workspace-id (.workspaceId r)
   :workspace-role (kw<- (.asString ^com.anthropic.models.organization.workspaces.WorkspaceRole (.workspaceRole r)))
   :created-by-actor-id (unopt (.createdByActorId r))
   :implicit (unopt (.implicit r))})

(defn add-service-account-workspace
  "Add a service account to a workspace with `:workspace-role` (a non-billing
  workspace role)."
  [^AnthropicClient client ^String service-account-id {:keys [workspace-id workspace-role]}]
  (with-api-errors
    (when-not workspace-id (missing-key! :workspace-id))
    (let [b (com.anthropic.models.organization.serviceaccounts.workspaces.WorkspaceAddParams/builder)]
      (.serviceAccountId b ^String service-account-id)
      (.workspaceId b ^String workspace-id)
      (when workspace-role
        (.workspaceRole b (com.anthropic.models.organization.workspaces.NoBillingWorkspaceRole/of
                           (->wire (check-enum! workspace-role no-billing-ws-roles :workspace-role)))))
      (sa-workspace-member->map (-> (.organization client) (.serviceAccounts)
                                    (.workspaces) (.add (.build b)))))))

(defn list-service-account-workspaces
  "List the workspaces a service account belongs to. Options: `:limit`, `:page`."
  ([^AnthropicClient client ^String service-account-id]
   (list-service-account-workspaces client service-account-id {}))
  ([^AnthropicClient client ^String service-account-id {:keys [limit page]}]
   (with-api-errors
     (let [b (com.anthropic.models.organization.serviceaccounts.workspaces.WorkspaceListParams/builder)]
       (.serviceAccountId b ^String service-account-id)
       (when limit (.limit b (long limit)))
       (when page (.page b ^String page))
       (mapv sa-workspace-member->map
             (.autoPager (-> (.organization client) (.serviceAccounts) (.workspaces)
                             (.list (.build b)))))))))

(defn remove-service-account-workspace
  "Remove a service account from a workspace. Returns the removal response map."
  [^AnthropicClient client ^String service-account-id ^String workspace-id]
  (with-api-errors
    (let [b (com.anthropic.models.organization.serviceaccounts.workspaces.WorkspaceRemoveParams/builder)]
      (.serviceAccountId b ^String service-account-id)
      (.workspaceId b ^String workspace-id)
      (obj->clj (-> (.organization client) (.serviceAccounts) (.workspaces)
                    (.remove (.build b)))))))

;; ---- Workspaces -----------------------------------------------------------

(defn- data-residency->map
  [^com.anthropic.models.organization.workspaces.DataResidency r]
  (let [allowed (.allowedInferenceGeos r)]
    {:allowed-inference-geos
     (cond
       (.isGeos allowed)
       (mapv #(kw<- (.asString
                     ^com.anthropic.models.organization.workspaces.AllowedInferenceGeo %))
             (.asGeos allowed))
       (.isUnrestricted allowed) :unrestricted)
     :default-inference-geo
     (kw<- (.asString
            ^com.anthropic.models.organization.workspaces.DataResidency$DefaultInferenceGeo
            (.defaultInferenceGeo r)))
     :workspace-geo
     (kw<- (.asString
            ^com.anthropic.models.organization.workspaces.DataResidency$WorkspaceGeo
            (.workspaceGeo r)))}))

(defn- workspace->map [^com.anthropic.models.organization.workspaces.Workspace r]
  {:id (.id r)
   :archived-at (some-> (.archivedAt r) unopt str)
   :compartment-id (.compartmentId r)
   :created-at (str (.createdAt r))
   :data-residency (data-residency->map (.dataResidency r))
   :display-color (.displayColor r)
   :external-key-id (unopt (.externalKeyId r))
   :name (.name r)
   :tags (obj->clj (.tags r))})

(defn create-workspace
  "Create a workspace. Requires `:name`. Optional `:display-color`,
  `:external-key-id`, and `:data-residency`/`:tags` as ready SDK config objects."
  [^AnthropicClient client {:keys [name display-color external-key-id data-residency tags]}]
  (with-api-errors
    (when-not name (missing-key! :name))
    (let [b (com.anthropic.models.organization.workspaces.WorkspaceCreateParams/builder)]
      (.name b ^String name)
      (when display-color (.displayColor b ^String display-color))
      (when external-key-id (.externalKeyId b ^String external-key-id))
      (when data-residency (.dataResidency b ^com.anthropic.models.organization.workspaces.DataResidencyCreateConfig data-residency))
      (when tags (.tags b ^com.anthropic.models.organization.workspaces.WorkspaceCreateParams$Tags tags))
      (workspace->map (-> (.organization client) (.workspaces) (.create (.build b)))))))

(defn get-workspace
  "Retrieve one workspace by id."
  [^AnthropicClient client ^String workspace-id]
  (with-api-errors
    (let [b (com.anthropic.models.organization.workspaces.WorkspaceRetrieveParams/builder)]
      (.workspaceId b ^String workspace-id)
      (workspace->map (-> (.organization client) (.workspaces) (.retrieve (.build b)))))))

(defn update-workspace
  "Update a workspace's `:name`, `:display-color`, `:external-key-id`, or
  `:data-residency`/`:tags` (ready SDK config objects)."
  [^AnthropicClient client ^String workspace-id {:keys [name display-color external-key-id data-residency tags]}]
  (with-api-errors
    (let [b (com.anthropic.models.organization.workspaces.WorkspaceUpdateParams/builder)]
      (.workspaceId b ^String workspace-id)
      (when name (.name b ^String name))
      (when display-color (.displayColor b ^String display-color))
      (when external-key-id (.externalKeyId b ^String external-key-id))
      (when data-residency (.dataResidency b ^com.anthropic.models.organization.workspaces.DataResidencyUpdateConfig data-residency))
      (when tags (.tags b ^com.anthropic.models.organization.workspaces.WorkspaceUpdateParams$Tags tags))
      (workspace->map (-> (.organization client) (.workspaces) (.update (.build b)))))))

(defn list-workspaces
  "List workspaces. Options: `:limit`, `:after-id`, `:before-id`,
  `:include-archived`."
  ([^AnthropicClient client] (list-workspaces client {}))
  ([^AnthropicClient client {:keys [limit after-id before-id include-archived]}]
   (with-api-errors
     (let [b (com.anthropic.models.organization.workspaces.WorkspaceListParams/builder)]
       (when limit (.limit b (long limit)))
       (when after-id (.afterId b ^String after-id))
       (when before-id (.beforeId b ^String before-id))
       (when (some? include-archived) (.includeArchived b (boolean include-archived)))
       (mapv workspace->map (.autoPager (-> (.organization client) (.workspaces)
                                            (.list (.build b)))))))))

(defn archive-workspace
  "Archive a workspace by id. Returns the archived workspace."
  [^AnthropicClient client ^String workspace-id]
  (with-api-errors
    (let [b (com.anthropic.models.organization.workspaces.WorkspaceArchiveParams/builder)]
      (.workspaceId b ^String workspace-id)
      (workspace->map (-> (.organization client) (.workspaces) (.archive (.build b)))))))

;; ---- Workspace members ----------------------------------------------------

(defn- workspace-member->map
  [^com.anthropic.models.organization.workspaces.WorkspaceMember r]
  {:user-id (.userId r)
   :workspace-id (.workspaceId r)
   :workspace-role (kw<- (.asString ^com.anthropic.models.organization.workspaces.WorkspaceRole (.workspaceRole r)))})

(defn get-workspace-member
  "Retrieve one workspace member by workspace id and user id."
  [^AnthropicClient client ^String workspace-id ^String user-id]
  (with-api-errors
    (let [b (com.anthropic.models.organization.workspaces.members.MemberRetrieveParams/builder)]
      (.workspaceId b ^String workspace-id)
      (.userId b ^String user-id)
      (workspace-member->map (-> (.organization client) (.workspaces) (.members)
                                 (.retrieve (.build b)))))))

(defn update-workspace-member
  "Update a workspace member's `:workspace-role` (any workspace role)."
  [^AnthropicClient client ^String workspace-id ^String user-id {:keys [workspace-role]}]
  (with-api-errors
    (let [b (com.anthropic.models.organization.workspaces.members.MemberUpdateParams/builder)]
      (.workspaceId b ^String workspace-id)
      (.userId b ^String user-id)
      (when workspace-role
        (.workspaceRole b (com.anthropic.models.organization.workspaces.WorkspaceRole/of
                           (->wire (check-enum! workspace-role ws-roles :workspace-role)))))
      (workspace-member->map (-> (.organization client) (.workspaces) (.members)
                                 (.update (.build b)))))))

(defn list-workspace-members
  "List a workspace's members. Options: `:limit`, `:after-id`, `:before-id`."
  ([^AnthropicClient client ^String workspace-id]
   (list-workspace-members client workspace-id {}))
  ([^AnthropicClient client ^String workspace-id {:keys [limit after-id before-id]}]
   (with-api-errors
     (let [b (com.anthropic.models.organization.workspaces.members.MemberListParams/builder)]
       (.workspaceId b ^String workspace-id)
       (when limit (.limit b (long limit)))
       (when after-id (.afterId b ^String after-id))
       (when before-id (.beforeId b ^String before-id))
       (mapv workspace-member->map
             (.autoPager (-> (.organization client) (.workspaces) (.members)
                             (.list (.build b)))))))))

(defn add-workspace-member
  "Add a user to a workspace with `:workspace-role` (a non-billing workspace
  role)."
  [^AnthropicClient client ^String workspace-id {:keys [user-id workspace-role]}]
  (with-api-errors
    (when-not user-id (missing-key! :user-id))
    (let [b (com.anthropic.models.organization.workspaces.members.MemberAddParams/builder)]
      (.workspaceId b ^String workspace-id)
      (.userId b ^String user-id)
      (when workspace-role
        (.workspaceRole b (com.anthropic.models.organization.workspaces.NoBillingWorkspaceRole/of
                           (->wire (check-enum! workspace-role no-billing-ws-roles :workspace-role)))))
      (workspace-member->map (-> (.organization client) (.workspaces) (.members)
                                 (.add (.build b)))))))

(defn remove-workspace-member
  "Remove a user from a workspace. Returns the removal response as a map."
  [^AnthropicClient client ^String workspace-id ^String user-id]
  (with-api-errors
    (let [b (com.anthropic.models.organization.workspaces.members.MemberRemoveParams/builder)]
      (.workspaceId b ^String workspace-id)
      (.userId b ^String user-id)
      (obj->clj (-> (.organization client) (.workspaces) (.members)
                    (.remove (.build b)))))))


(defn- workspace-rate-limit->map
  [^com.anthropic.models.organization.workspaces.ratelimits.WorkspaceRateLimit r]
  (cond-> {:rate-limit-id (.rateLimitId r)
           :workspace-id (.workspaceId r)
           :group (->rate-limit-group (.group r))
           :limits (mapv (fn [^com.anthropic.models.organization.workspaces.ratelimits.WorkspaceRateLimitValue limit]
                           (cond-> {:type (.type limit)
                                    :value (.value limit)}
                             (some? (unopt (.orgLimit limit)))
                             (assoc :org-limit (unopt (.orgLimit limit)))
                             (some? (unopt (.source limit)))
                             (assoc :source (obj->clj (unopt (.source limit))))))
                        (.limits r))}
    (some? (unopt (.models r)))
    (assoc :models (vec (unopt (.models r))))))

;; ---- Workspace rate limits ------------------------------------------------

(defn- ->workspace-rate-limit-list-params
  [workspace-id {:keys [limit page group-type include-inherited]}]
  (let [b (com.anthropic.models.organization.workspaces.ratelimits.RateLimitListParams/builder)]
    (.workspaceId b ^String workspace-id)
    (when limit (.limit b (long limit)))
    (when page (.page b ^String page))
    (when (some? include-inherited) (.includeInherited b (boolean include-inherited)))
    (when group-type
      (.groupType b (com.anthropic.models.organization.workspaces.ratelimits.RateLimitListParams$GroupType/of
                    (->wire (check-enum! group-type group-types :group-type)))))
    (.build b)))

(defn list-workspace-rate-limits
  "List a workspace's rate limits. Options: `:limit`, `:page`, `:group-type`,
  and `:include-inherited`."
  ([^AnthropicClient client ^String workspace-id]
   (list-workspace-rate-limits client workspace-id {}))
  ([^AnthropicClient client ^String workspace-id opts]
   (with-api-errors
     (let [params (->workspace-rate-limit-list-params workspace-id opts)]
       (mapv workspace-rate-limit->map (.autoPager (-> (.organization client) (.workspaces) (.rateLimits)
                                      (.list params))))))))

;; ---- Workspace service accounts -------------------------------------------

(defn get-workspace-service-account
  "Retrieve a service account's membership in a workspace."
  [^AnthropicClient client ^String workspace-id ^String service-account-id]
  (with-api-errors
    (let [b (com.anthropic.models.organization.workspaces.serviceaccounts.ServiceAccountRetrieveParams/builder)]
      (.workspaceId b ^String workspace-id)
      (.serviceAccountId b ^String service-account-id)
      (sa-workspace-member->map (-> (.organization client) (.workspaces) (.serviceAccounts)
                                    (.retrieve (.build b)))))))

(defn update-workspace-service-account
  "Update a workspace service account's `:workspace-role` (a non-billing role)."
  [^AnthropicClient client ^String workspace-id ^String service-account-id {:keys [workspace-role]}]
  (with-api-errors
    (let [b (com.anthropic.models.organization.workspaces.serviceaccounts.ServiceAccountUpdateParams/builder)]
      (.workspaceId b ^String workspace-id)
      (.serviceAccountId b ^String service-account-id)
      (when workspace-role
        (.workspaceRole b (com.anthropic.models.organization.workspaces.NoBillingWorkspaceRole/of
                           (->wire (check-enum! workspace-role no-billing-ws-roles :workspace-role)))))
      (sa-workspace-member->map (-> (.organization client) (.workspaces) (.serviceAccounts)
                                    (.update (.build b)))))))

(defn list-workspace-service-accounts
  "List a workspace's service accounts. Options: `:limit`, `:page`."
  ([^AnthropicClient client ^String workspace-id]
   (list-workspace-service-accounts client workspace-id {}))
  ([^AnthropicClient client ^String workspace-id {:keys [limit page]}]
   (with-api-errors
     (let [b (com.anthropic.models.organization.workspaces.serviceaccounts.ServiceAccountListParams/builder)]
       (.workspaceId b ^String workspace-id)
       (when limit (.limit b (long limit)))
       (when page (.page b ^String page))
       (mapv sa-workspace-member->map
             (.autoPager (-> (.organization client) (.workspaces) (.serviceAccounts)
                             (.list (.build b)))))))))

(defn add-workspace-service-account
  "Add a service account to a workspace with `:workspace-role` (non-billing)."
  [^AnthropicClient client ^String workspace-id {:keys [service-account-id workspace-role]}]
  (with-api-errors
    (when-not service-account-id (missing-key! :service-account-id))
    (let [b (com.anthropic.models.organization.workspaces.serviceaccounts.ServiceAccountAddParams/builder)]
      (.workspaceId b ^String workspace-id)
      (.serviceAccountId b ^String service-account-id)
      (when workspace-role
        (.workspaceRole b (com.anthropic.models.organization.workspaces.NoBillingWorkspaceRole/of
                           (->wire (check-enum! workspace-role no-billing-ws-roles :workspace-role)))))
      (sa-workspace-member->map (-> (.organization client) (.workspaces) (.serviceAccounts)
                                    (.add (.build b)))))))

(defn remove-workspace-service-account
  "Remove a service account from a workspace. Returns the removal response map."
  [^AnthropicClient client ^String workspace-id ^String service-account-id]
  (with-api-errors
    (let [b (com.anthropic.models.organization.workspaces.serviceaccounts.ServiceAccountRemoveParams/builder)]
      (.workspaceId b ^String workspace-id)
      (.serviceAccountId b ^String service-account-id)
      (obj->clj (-> (.organization client) (.workspaces) (.serviceAccounts)
                    (.remove (.build b)))))))

;; ---- Federation issuers ---------------------------------------------------

(defn- federation-issuer->map
  [^com.anthropic.models.organization.federation.issuers.FederationIssuer r]
  {:id (.id r)
   :archived-at (some-> (.archivedAt r) unopt str)
   :archived-by-actor-id (unopt (.archivedByActorId r))
   :check-jti (.checkJti r)
   :created-at (str (.createdAt r))
   :created-by-actor-id (unopt (.createdByActorId r))
   :issuer-url (.issuerUrl r)
   :jwks (obj->clj (.jwks r))
   :jwks-polling-disabled-at (some-> (.jwksPollingDisabledAt r) unopt str)
   :max-jwt-lifetime-seconds (.maxJwtLifetimeSeconds r)
   :name (.name r)
   :poll-status (obj->clj (unopt (.pollStatus r)))
   :updated-at (str (.updatedAt r))
   :updated-by-actor-id (unopt (.updatedByActorId r))})

(defn create-federation-issuer
  "Create a federation issuer. Requires `:name` and `:issuer-url`. JWKS via
  `:explicit-url-jwks` (a URL string) or `:jwks` (a ready SDK JWKS object).
  Optional `:check-jti`, `:max-jwt-lifetime-seconds`."
  [^AnthropicClient client {:keys [name issuer-url explicit-url-jwks jwks check-jti max-jwt-lifetime-seconds]}]
  (with-api-errors
    (when-not name (missing-key! :name))
    (when-not issuer-url (missing-key! :issuer-url))
    (let [b (com.anthropic.models.organization.federation.issuers.IssuerCreateParams/builder)]
      (.name b ^String name)
      (.issuerUrl b ^String issuer-url)
      (when explicit-url-jwks (.explicitUrlJwks b ^String explicit-url-jwks))
      (when jwks (.jwks b ^com.anthropic.models.organization.federation.issuers.IssuerCreateParams$Jwks jwks))
      (when (some? check-jti) (.checkJti b (boolean check-jti)))
      (when max-jwt-lifetime-seconds (.maxJwtLifetimeSeconds b (long max-jwt-lifetime-seconds)))
      (federation-issuer->map (-> (.organization client) (.federation) (.issuers)
                                  (.create (.build b)))))))

(defn get-federation-issuer
  "Retrieve one federation issuer by id."
  [^AnthropicClient client ^String issuer-id]
  (with-api-errors
    (let [b (com.anthropic.models.organization.federation.issuers.IssuerRetrieveParams/builder)]
      (.federationIssuerId b ^String issuer-id)
      (federation-issuer->map (-> (.organization client) (.federation) (.issuers)
                                  (.retrieve (.build b)))))))

(defn update-federation-issuer
  "Update a federation issuer's `:name`, `:issuer-url`, JWKS
  (`:explicit-url-jwks`/`:jwks`), `:check-jti`, `:max-jwt-lifetime-seconds`, or
  `:jwks-polling-disabled`."
  [^AnthropicClient client ^String issuer-id
   {:keys [name issuer-url explicit-url-jwks jwks check-jti max-jwt-lifetime-seconds jwks-polling-disabled]}]
  (with-api-errors
    (let [b (com.anthropic.models.organization.federation.issuers.IssuerUpdateParams/builder)]
      (.federationIssuerId b ^String issuer-id)
      (when name (.name b ^String name))
      (when issuer-url (.issuerUrl b ^String issuer-url))
      (when explicit-url-jwks (.explicitUrlJwks b ^String explicit-url-jwks))
      (when jwks (.jwks b ^com.anthropic.models.organization.federation.issuers.IssuerUpdateParams$Jwks jwks))
      (when (some? check-jti) (.checkJti b (boolean check-jti)))
      (when max-jwt-lifetime-seconds (.maxJwtLifetimeSeconds b (long max-jwt-lifetime-seconds)))
      (when (some? jwks-polling-disabled) (.jwksPollingDisabled b (boolean jwks-polling-disabled)))
      (federation-issuer->map (-> (.organization client) (.federation) (.issuers)
                                  (.update (.build b)))))))

(defn list-federation-issuers
  "List federation issuers. Options: `:limit`, `:page`, `:include-archived`."
  ([^AnthropicClient client] (list-federation-issuers client {}))
  ([^AnthropicClient client {:keys [limit page include-archived]}]
   (with-api-errors
     (let [b (com.anthropic.models.organization.federation.issuers.IssuerListParams/builder)]
       (when limit (.limit b (long limit)))
       (when page (.page b ^String page))
       (when (some? include-archived) (.includeArchived b (boolean include-archived)))
       (mapv federation-issuer->map
             (.autoPager (-> (.organization client) (.federation) (.issuers)
                             (.list (.build b)))))))))

(defn archive-federation-issuer
  "Archive a federation issuer by id. Returns the archived issuer."
  [^AnthropicClient client ^String issuer-id]
  (with-api-errors
    (let [b (com.anthropic.models.organization.federation.issuers.IssuerArchiveParams/builder)]
      (.federationIssuerId b ^String issuer-id)
      (federation-issuer->map (-> (.organization client) (.federation) (.issuers)
                                  (.archive (.build b)))))))

;; ---- Federation rules -----------------------------------------------------

(defn- federation-rule->map
  [^com.anthropic.models.beta.organization.federation.rules.BetaFederationRule r]
  {:id (.id r)
   :applies-to-all-workspaces (.appliesToAllWorkspaces r)
   :archived-at (some-> (.archivedAt r) unopt str)
   :archived-by-actor-id (unopt (.archivedByActorId r))
   :attributes (obj->clj (unopt (.attributes r)))
   :created-at (str (.createdAt r))
   :created-by-actor-id (unopt (.createdByActorId r))
   :description (unopt (.description r))
   :issuer-id (.issuerId r)
   :issuer-name (unopt (.issuerName r))
   :match (obj->clj (.match r))
   :name (.name r)
   :oauth-scope (.oauthScope r)
   :target (obj->clj (.target r))
   :token-lifetime-seconds (.tokenLifetimeSeconds r)
   :updated-at (str (.updatedAt r))
   :updated-by-actor-id (unopt (.updatedByActorId r))
   :workspace-id (unopt (.workspaceId r))
   :workspace-ids (vec (.workspaceIds r))})

(defn- ->target
  ^com.anthropic.models.beta.organization.federation.rules.BetaServiceAccountTarget
  [target]
  (if (instance? com.anthropic.models.beta.organization.federation.rules.BetaServiceAccountTarget target)
    target
    (let [{:keys [service-account-id service-account-name]} target
          b (com.anthropic.models.beta.organization.federation.rules.BetaServiceAccountTarget/builder)]
      (.serviceAccountId b ^String service-account-id)
      (when service-account-name (.serviceAccountName b ^String service-account-name))
      (.build b))))

(defn- ->match
  ^com.anthropic.models.beta.organization.federation.rules.BetaFederationRuleMatch
  [match]
  (if (instance? com.anthropic.models.beta.organization.federation.rules.BetaFederationRuleMatch match)
    match
    (let [{:keys [audience condition subject-prefix claims]} match
          b (com.anthropic.models.beta.organization.federation.rules.BetaFederationRuleMatch/builder)]
      (when audience (.audience b ^String audience))
      (when condition (.condition b ^String condition))
      (when subject-prefix (.subjectPrefix b ^String subject-prefix))
      (when claims (.claims b ^com.anthropic.models.beta.organization.federation.rules.BetaFederationRuleMatch$Claims claims))
      (.build b))))

(defn create-federation-rule
  "Create a federation rule. Requires `:name`, `:issuer-id`, `:oauth-scope`,
  `:token-lifetime-seconds`, and `:target` (a map `{:service-account-id ...}`
  or a ready SDK target). Optional `:description`, `:applies-to-all-workspaces`,
  `:workspace-id`, and `:match` (a map of `:audience`/`:condition`/
  `:subject-prefix`/`:claims`, or a ready SDK match)."
  [^AnthropicClient client
   {:keys [name issuer-id oauth-scope token-lifetime-seconds target description
           applies-to-all-workspaces workspace-id match]}]
  (with-api-errors
    (when-not name (missing-key! :name))
    (when-not issuer-id (missing-key! :issuer-id))
    (when-not oauth-scope (missing-key! :oauth-scope))
    (when-not token-lifetime-seconds (missing-key! :token-lifetime-seconds))
    (when-not target (missing-key! :target))
    (let [b (com.anthropic.models.beta.organization.federation.rules.RuleCreateParams/builder)]
      (.name b ^String name)
      (.issuerId b ^String issuer-id)
      (.oauthScope b ^String oauth-scope)
      (.tokenLifetimeSeconds b (long token-lifetime-seconds))
      (.target b (->target target))
      (when description (.description b ^String description))
      (when (some? applies-to-all-workspaces) (.appliesToAllWorkspaces b (boolean applies-to-all-workspaces)))
      (when workspace-id (.workspaceId b ^String workspace-id))
      (when match (.match b (->match match)))
      (federation-rule->map (-> (.beta client) (.organization) (.federation) (.rules)
                                (.create (.build b)))))))

(defn get-federation-rule
  "Retrieve one federation rule by id."
  [^AnthropicClient client ^String rule-id]
  (with-api-errors
    (let [b (com.anthropic.models.beta.organization.federation.rules.RuleRetrieveParams/builder)]
      (.federationRuleId b ^String rule-id)
      (federation-rule->map (-> (.beta client) (.organization) (.federation) (.rules)
                                (.retrieve (.build b)))))))

(defn update-federation-rule
  "Update a federation rule's `:name`, `:description`, `:oauth-scope`,
  `:token-lifetime-seconds`, `:applies-to-all-workspaces`, `:workspace-id`,
  `:target`, or `:match`."
  [^AnthropicClient client ^String rule-id
   {:keys [name description oauth-scope token-lifetime-seconds applies-to-all-workspaces
           workspace-id target match]}]
  (with-api-errors
    (let [b (com.anthropic.models.beta.organization.federation.rules.RuleUpdateParams/builder)]
      (.federationRuleId b ^String rule-id)
      (when name (.name b ^String name))
      (when description (.description b ^String description))
      (when oauth-scope (.oauthScope b ^String oauth-scope))
      (when token-lifetime-seconds (.tokenLifetimeSeconds b (long token-lifetime-seconds)))
      (when (some? applies-to-all-workspaces) (.appliesToAllWorkspaces b (boolean applies-to-all-workspaces)))
      (when workspace-id (.workspaceId b ^String workspace-id))
      (when target (.target b (->target target)))
      (when match (.match b (->match match)))
      (federation-rule->map (-> (.beta client) (.organization) (.federation) (.rules)
                                (.update (.build b)))))))

(defn list-federation-rules
  "List federation rules. Options: `:limit`, `:page`, `:include-archived`,
  `:issuer-id`."
  ([^AnthropicClient client] (list-federation-rules client {}))
  ([^AnthropicClient client {:keys [limit page include-archived issuer-id]}]
   (with-api-errors
     (let [b (com.anthropic.models.beta.organization.federation.rules.RuleListParams/builder)]
       (when limit (.limit b (long limit)))
       (when page (.page b ^String page))
       (when (some? include-archived) (.includeArchived b (boolean include-archived)))
       (when issuer-id (.issuerId b ^String issuer-id))
       (mapv federation-rule->map
             (.autoPager (-> (.beta client) (.organization) (.federation) (.rules)
                             (.list (.build b)))))))))

(defn archive-federation-rule
  "Archive a federation rule by id. Returns the archived rule."
  [^AnthropicClient client ^String rule-id]
  (with-api-errors
    (let [b (com.anthropic.models.beta.organization.federation.rules.RuleArchiveParams/builder)]
      (.federationRuleId b ^String rule-id)
      (federation-rule->map (-> (.beta client) (.organization) (.federation) (.rules)
                                (.archive (.build b)))))))

;; ---- Federation rule workspaces -------------------------------------------

(defn- rule-workspace->map
  [^com.anthropic.models.organization.federation.rules.FederationRuleWorkspace r]
  {:federation-rule-id (.federationRuleId r)
   :workspace-id (.workspaceId r)
   :created-at (str (.createdAt r))
   :created-by-actor-id (unopt (.createdByActorId r))
   :workspace-name (unopt (.workspaceName r))})

(defn add-federation-rule-workspace
  "Attach a workspace to a federation rule."
  [^AnthropicClient client ^String rule-id ^String workspace-id]
  (with-api-errors
    (let [b (com.anthropic.models.organization.federation.rules.workspaces.WorkspaceAddParams/builder)]
      (.federationRuleId b ^String rule-id)
      (.workspaceId b ^String workspace-id)
      (rule-workspace->map (-> (.organization client) (.federation) (.rules) (.workspaces)
                               (.add (.build b)))))))

(defn list-federation-rule-workspaces
  "List the workspaces attached to a federation rule. Options: `:limit`, `:page`."
  ([^AnthropicClient client ^String rule-id]
   (list-federation-rule-workspaces client rule-id {}))
  ([^AnthropicClient client ^String rule-id {:keys [limit page]}]
   (with-api-errors
     (let [b (com.anthropic.models.organization.federation.rules.workspaces.WorkspaceListParams/builder)]
       (.federationRuleId b ^String rule-id)
       (when limit (.limit b (long limit)))
       (when page (.page b ^String page))
       (mapv rule-workspace->map
             (.autoPager (-> (.organization client) (.federation) (.rules) (.workspaces)
                             (.list (.build b)))))))))

(defn remove-federation-rule-workspace
  "Detach a workspace from a federation rule. Returns the removal response map."
  [^AnthropicClient client ^String rule-id ^String workspace-id]
  (with-api-errors
    (let [b (com.anthropic.models.organization.federation.rules.workspaces.WorkspaceRemoveParams/builder)]
      (.federationRuleId b ^String rule-id)
      (.workspaceId b ^String workspace-id)
      (obj->clj (-> (.organization client) (.federation) (.rules) (.workspaces)
                    (.remove (.build b)))))))

;; ---- Beta-only administration --------------------------------------------

(defn- analytics [client] (dynamic-call (beta-organization client) "analytics"))
(defn- analytics-list [client path params opts]
  (dynamic-list (reduce dynamic-call (analytics client) path)
                (->dynamic-params params (or opts {}))))

(defn list-analytics-summaries [client opts]
  (with-api-errors (analytics-list client ["summaries"] "com.anthropic.models.beta.organization.analytics.summaries.SummaryListParams" opts)))
(defn list-analytics-users
  ([client] (list-analytics-users client {}))
  ([client opts] (with-api-errors (analytics-list client ["users"] "com.anthropic.models.beta.organization.analytics.users.UserListParams" opts))))
(defn list-analytics-chat-projects
  ([client] (list-analytics-chat-projects client {}))
  ([client opts] (with-api-errors (analytics-list client ["apps" "chat" "projects"] "com.anthropic.models.beta.organization.analytics.apps.chat.projects.ProjectListParams" opts))))
(defn list-analytics-connectors
  ([client] (list-analytics-connectors client {}))
  ([client opts] (with-api-errors (analytics-list client ["connectors"] "com.anthropic.models.beta.organization.analytics.connectors.ConnectorListParams" opts))))
(defn list-analytics-plugins
  ([client] (list-analytics-plugins client {}))
  ([client opts] (with-api-errors (analytics-list client ["plugins"] "com.anthropic.models.beta.organization.analytics.plugins.PluginListParams" opts))))
(defn list-analytics-skills
  ([client] (list-analytics-skills client {}))
  ([client opts] (with-api-errors (analytics-list client ["skills"] "com.anthropic.models.beta.organization.analytics.skills.SkillListParams" opts))))
(defn list-analytics-artifacts [client opts]
  (with-api-errors (analytics-list client ["artifacts"] "com.anthropic.models.beta.organization.analytics.artifacts.ArtifactListParams" opts)))
(defn list-analytics-usage-report [client opts]
  (with-api-errors (analytics-list client ["usageReport"] "com.anthropic.models.beta.organization.analytics.usagereport.UsageReportListParams" opts)))
(defn list-analytics-user-usage-report [client opts]
  (with-api-errors (analytics-list client ["userUsageReport"] "com.anthropic.models.beta.organization.analytics.userusagereport.UserUsageReportListParams" opts)))
(defn list-analytics-cost-report [client opts]
  (with-api-errors (analytics-list client ["costReport"] "com.anthropic.models.beta.organization.analytics.costreport.CostReportListParams" opts)))
(defn list-analytics-user-cost-report [client opts]
  (with-api-errors (analytics-list client ["userCostReport"] "com.anthropic.models.beta.organization.analytics.usercostreport.UserCostReportListParams" opts)))

(defn- admin-call [client service method params opts]
  (obj->clj (dynamic-call (dynamic-call (beta-organization client) service)
                          method (->dynamic-params params opts))))
(defn- admin-list [client service params opts]
  (dynamic-list (dynamic-call (beta-organization client) service)
                (->dynamic-params params (or opts {}))))

(defn get-spend-limit [client id] (with-api-errors (admin-call client "spendLimits" "retrieve" "com.anthropic.models.beta.organization.spendlimits.SpendLimitRetrieveParams" {:spend-limit-id id})))
(defn list-spend-limits ([client] (list-spend-limits client {})) ([client opts] (with-api-errors (admin-list client "spendLimits" "com.anthropic.models.beta.organization.spendlimits.SpendLimitListParams" opts))))
(defn delete-spend-limit [client id] (with-api-errors (admin-call client "spendLimits" "delete" "com.anthropic.models.beta.organization.spendlimits.SpendLimitDeleteParams" {:spend-limit-id id})))
(defn set-spend-limit [client changes] (with-api-errors (admin-call client "spendLimits" "set" "com.anthropic.models.beta.organization.spendlimits.SpendLimitSetParams" changes)))

(defn create-rbac-group [client changes] (with-api-errors (admin-call client "rbacGroups" "create" "com.anthropic.models.beta.organization.rbacgroups.RbacGroupCreateParams" changes)))
(defn get-rbac-group [client id] (with-api-errors (admin-call client "rbacGroups" "retrieve" "com.anthropic.models.beta.organization.rbacgroups.RbacGroupRetrieveParams" {:rbac-group-id id})))
(defn update-rbac-group [client id changes] (with-api-errors (admin-call client "rbacGroups" "update" "com.anthropic.models.beta.organization.rbacgroups.RbacGroupUpdateParams" (assoc changes :rbac-group-id id))))
(defn list-rbac-groups ([client] (list-rbac-groups client {})) ([client opts] (with-api-errors (admin-list client "rbacGroups" "com.anthropic.models.beta.organization.rbacgroups.RbacGroupListParams" opts))))
(defn delete-rbac-group [client id] (with-api-errors (admin-call client "rbacGroups" "delete" "com.anthropic.models.beta.organization.rbacgroups.RbacGroupDeleteParams" {:rbac-group-id id})))
(defn get-rbac-role [client id] (with-api-errors (admin-call client "rbacRoles" "retrieve" "com.anthropic.models.beta.organization.rbacroles.RbacRoleRetrieveParams" {:rbac-role-id id})))
(defn list-rbac-roles ([client] (list-rbac-roles client {})) ([client opts] (with-api-errors (admin-list client "rbacRoles" "com.anthropic.models.beta.organization.rbacroles.RbacRoleListParams" opts))))

(defn create-plugin [client changes] (with-api-errors (admin-call client "plugins" "create" "com.anthropic.models.beta.organization.plugins.PluginCreateParams" changes)))
(defn get-plugin [client id] (with-api-errors (admin-call client "plugins" "retrieve" "com.anthropic.models.beta.organization.plugins.PluginRetrieveParams" {:plugin-id id})))
(defn update-plugin [client id changes] (with-api-errors (admin-call client "plugins" "update" "com.anthropic.models.beta.organization.plugins.PluginUpdateParams" (assoc changes :plugin-id id))))
(defn list-plugins ([client] (list-plugins client {})) ([client opts] (with-api-errors (admin-list client "plugins" "com.anthropic.models.beta.organization.plugins.PluginListParams" opts))))
(defn delete-plugin [client id] (with-api-errors (admin-call client "plugins" "delete" "com.anthropic.models.beta.organization.plugins.PluginDeleteParams" {:plugin-id id})))

(defn- plugin-installation-settings [client]
  (dynamic-call (dynamic-call (beta-organization client) "plugins") "installationSettings"))
(defn list-plugin-installation-settings
  ([client plugin-id] (list-plugin-installation-settings client plugin-id {}))
  ([client plugin-id opts] (with-api-errors (dynamic-list (plugin-installation-settings client) (->dynamic-params "com.anthropic.models.beta.organization.plugins.installationsettings.InstallationSettingListParams" (assoc opts :plugin-id plugin-id))))))
(defn remove-plugin-installation-setting
  "Remove the installation setting of `plugin-id` for `target` (the id of the
  organization member, RBAC group, etc. the setting applies to)."
  [client plugin-id target]
  (with-api-errors (obj->clj (dynamic-call (plugin-installation-settings client) "remove" (->dynamic-params "com.anthropic.models.beta.organization.plugins.installationsettings.InstallationSettingRemoveParams" {:plugin-id plugin-id :target target})))))
(defn set-plugin-installation-setting [client plugin-id changes]
  (with-api-errors (obj->clj (dynamic-call (plugin-installation-settings client) "set" (->dynamic-params "com.anthropic.models.beta.organization.plugins.installationsettings.InstallationSettingSetParams" (assoc changes :plugin-id plugin-id))))))

(defn get-plugin-marketplace [client id] (with-api-errors (admin-call client "pluginMarketplaces" "retrieve" "com.anthropic.models.beta.organization.pluginmarketplaces.PluginMarketplaceRetrieveParams" {:marketplace-id id})))
(defn update-plugin-marketplace [client id changes] (with-api-errors (admin-call client "pluginMarketplaces" "update" "com.anthropic.models.beta.organization.pluginmarketplaces.PluginMarketplaceUpdateParams" (assoc changes :marketplace-id id))))
(defn list-plugin-marketplaces ([client] (list-plugin-marketplaces client {})) ([client opts] (with-api-errors (admin-list client "pluginMarketplaces" "com.anthropic.models.beta.organization.pluginmarketplaces.PluginMarketplaceListParams" opts))))
(defn validate-plugin-marketplace-archive [client archive] (with-api-errors (admin-call client "pluginMarketplaces" "validateArchive" "com.anthropic.models.beta.organization.pluginmarketplaces.PluginMarketplaceValidateArchiveParams" {:archive archive})))
(defn validate-plugin-marketplace-repository [client repository-url opts] (with-api-errors (admin-call client "pluginMarketplaces" "validateRepository" "com.anthropic.models.beta.organization.pluginmarketplaces.PluginMarketplaceValidateRepositoryParams" (assoc (or opts {}) :repository-url repository-url))))
