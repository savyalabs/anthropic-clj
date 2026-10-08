(ns anthropic.organization-test
  (:require [clojure.test :refer [deftest is testing]]
            [anthropic.organization]
            [anthropic.organization-test-support :as support])
  (:import
           (com.anthropic.models.organization.users OrganizationUser)
           (com.anthropic.models.organization OrganizationRole)
           (com.anthropic.models.organization.workspaces AllowedInferenceGeo
                                                              DataResidency
                                                              DataResidency$DefaultInferenceGeo
                                                              DataResidency$WorkspaceGeo
                                                              Workspace
                                                              Workspace$Tags
                                                              WorkspaceMember
                                                              WorkspaceRole)
           (com.anthropic.models.beta.organization.federation.rules BetaServiceAccountTarget
                                                                    BetaFederationRuleMatch)
           (java.time OffsetDateTime)
           (java.util Optional)))

(defn- private-fn [sym]
  (let [v (ns-resolve 'anthropic.organization sym)]
    (when v @v)))

(deftest workspace-list-params-support-include-default
  (let [build-params (private-fn '->workspace-list-params)
        true-params (build-params {:include-default true})
        false-params (build-params {:include-default false})
        absent-params (build-params {})]
    (is (= true (.orElse (.includeDefault true-params) nil)))
    (is (= false (.orElse (.includeDefault false-params) nil)))
    (is (= nil (.orElse (.includeDefault absent-params) nil)))))

(deftest workspace-rate-limit-list-params-support-inherited-rates
  (let [build-params (private-fn '->workspace-rate-limit-list-params)]
    (is (some? build-params))
    (when build-params
      (let [params (build-params "ws_1" {:include-inherited true})]
        (is (true? (.orElse (.includeInherited params) false)))))))

(deftest compliance-settings-conversion
  (let [->params (private-fn '->compliance-update-params)
        convert (private-fn 'compliance-settings->map)
        ^com.anthropic.models.organization.compliancesettings.ComplianceSettingUpdateParams p
        (->params {:state :enabled})
        enabled (.build (com.anthropic.models.organization.compliancesettings.ComplianceSettingsStateEnabled/builder))
        response (-> (com.anthropic.models.organization.compliancesettings.OrganizationComplianceSettings/builder)
                     (.state (com.anthropic.models.organization.compliancesettings.ComplianceSettingsState/ofEnabled enabled))
                     (.build))]
    (is (.isEnabled (.state p)))
    (is (= {:state :enabled} (convert response)))
    (is (= {:state :disabled}
           (convert
            (-> (com.anthropic.models.organization.compliancesettings.OrganizationComplianceSettings/builder)
                (.state (com.anthropic.models.organization.compliancesettings.ComplianceSettingsState/ofDisabled
                         (.build (com.anthropic.models.organization.compliancesettings.ComplianceSettingsStateDisabled/builder))))
                (.build)))))))

;; ---- enum coercion --------------------------------------------------------

(deftest ->wire-produces-lower-snake
  (let [->wire (private-fn '->wire)]
    (is (= "primary_owner" (->wire :primary-owner)))
    (is (= "workspace_admin" (->wire :workspace-admin)))
    (is (= "admin" (->wire :admin)))))

(deftest check-enum!-accepts-and-rejects
  (let [check-enum! (private-fn 'check-enum!)
        roles (private-fn 'ws-roles)]
    (is (= :workspace-admin (check-enum! :workspace-admin roles :role)))
    (is (= :workspace-user (check-enum! "workspace_user" roles :role)) "coerces string to keyword")
    (is (thrown? clojure.lang.ExceptionInfo (check-enum! :nope roles :role)))
    (try (check-enum! :nope roles :role)
         (catch clojure.lang.ExceptionInfo e
           (is (= :invalid-enum-value (:anthropic/error (ex-data e))))))))

;; ---- federation target/match builders from maps ---------------------------

(deftest ->target-builds-from-map
  (let [->target (private-fn '->target)
        ^BetaServiceAccountTarget t (->target {:service-account-id "sa_1"
                                               :service-account-name "svc"})]
    (is (= "sa_1" (.serviceAccountId t)))
    (is (= "svc" (.orElse (.serviceAccountName t) nil)))))

(deftest ->target-passes-through-sdk-object
  (let [->target (private-fn '->target)
        obj (-> (BetaServiceAccountTarget/builder) (.serviceAccountId "sa_2") (.build))]
    (is (identical? obj (->target obj)) "an SDK target is used as-is")))

(deftest ->match-builds-scalars
  (let [->match (private-fn '->match)
        ^BetaFederationRuleMatch m (->match {:audience "aud" :condition "cond"
                                             :subject-prefix "pre"})]
    (is (= "aud" (.orElse (.audience m) nil)))
    (is (= "cond" (.orElse (.condition m) nil)))
    (is (= "pre" (.orElse (.subjectPrefix m) nil)))))

;; ---- response converters --------------------------------------------------

(deftest org-user->map-shape
  (let [conv (private-fn 'org-user->map)
        now (OffsetDateTime/parse "2026-08-26T00:00:00Z")
        u (-> (OrganizationUser/builder)
              (.id "user_1")
              (.addedAt now)
              (.email "a@b.c")
              (.name "Ada")
              (.role (OrganizationRole/of "primary_owner"))
              (.build))
        m (conv u)]
    (is (= "user_1" (:id m)))
    (is (= "a@b.c" (:email m)))
    (is (= "Ada" (:name m)))
    (is (= :primary-owner (:role m)) "wire enum decoded to kebab keyword")
    (is (string? (:added-at m)))))

(deftest workspace-member->map-shape
  (let [conv (private-fn 'workspace-member->map)
        wm (-> (WorkspaceMember/builder)
               (.userId "user_9")
               (.workspaceId "wrkspc_9")
               (.workspaceRole (WorkspaceRole/of "workspace_developer"))
               (.build))
        m (conv wm)]
    (is (= "user_9" (:user-id m)))
    (is (= "wrkspc_9" (:workspace-id m)))
    (is (= :workspace-developer (:workspace-role m)))))

(deftest workspace-data-residency-enums-are-keywords
  (let [convert (private-fn 'workspace->map)
        data-residency (-> (DataResidency/builder)
                           (.allowedInferenceGeosOfGeos
                            [(AllowedInferenceGeo/of "global")
                             (AllowedInferenceGeo/of "us")])
                           (.defaultInferenceGeo
                            (DataResidency$DefaultInferenceGeo/of "global"))
                           (.workspaceGeo (DataResidency$WorkspaceGeo/of "us"))
                           .build)
        workspace (-> (Workspace/builder)
                      (.id "wrkspc_1")
                      (.archivedAt (Optional/empty))
                      (.compartmentId "compartment_1")
                      (.createdAt (OffsetDateTime/parse "2026-09-16T00:00:00Z"))
                      (.dataResidency data-residency)
                      (.displayColor "#123456")
                      (.externalKeyId (Optional/empty))
                      (.name "US workspace")
                      (.tags (.build (Workspace$Tags/builder)))
                      .build)]
    (is (= {:allowed-inference-geos [:global :us]
            :default-inference-geo :global
            :workspace-geo :us}
           (:data-residency (convert workspace))))))

(deftest rate-limit-group-roundtrips-through-custom-conversion
  (let [rate-limit->map (private-fn 'rate-limit->map)
        rate-limit (-> (com.anthropic.models.organization.ratelimits.OrganizationRateLimit/builder)
                       (.id "rl_1")
                       (.group (-> (com.anthropic.models.organization.ratelimits.OrganizationRateLimitModelGroup/builder)
                                   (.id "model-group-1")
                                   (.displayName "Claude models")
                                   (.build)))
                       (.limits [])
                       (.models [])
                       (.build))]
    (is (= {:id "model-group-1"
            :display-name "Claude models"
            :type "model-group"}
           (:group (rate-limit->map rate-limit))))))

;; ---- public surface -------------------------------------------------------

(deftest public-api-covers-every-service
  (testing "one idiomatic fn per Organization operation is present"
    (doseq [s '[get-organization
                get-org-user update-org-user list-org-users remove-org-user
                get-api-key update-api-key list-api-keys
                create-external-key get-external-key update-external-key
                list-external-keys delete-external-key validate-external-key
                create-invite get-invite list-invites delete-invite
                list-rate-limits
                create-service-account get-service-account update-service-account
                list-service-accounts archive-service-account
                add-service-account-workspace list-service-account-workspaces
                remove-service-account-workspace
                create-workspace get-workspace update-workspace list-workspaces
                archive-workspace
                get-workspace-member update-workspace-member list-workspace-members
                add-workspace-member remove-workspace-member
                list-workspace-rate-limits
                get-workspace-service-account update-workspace-service-account
                list-workspace-service-accounts add-workspace-service-account
                remove-workspace-service-account
                create-federation-issuer get-federation-issuer update-federation-issuer
                list-federation-issuers archive-federation-issuer
                create-federation-rule get-federation-rule update-federation-rule
                list-federation-rules archive-federation-rule
                add-federation-rule-workspace list-federation-rule-workspaces
                remove-federation-rule-workspace]]
      (is (ifn? (ns-resolve 'anthropic.organization s))
          (str s " must be a public fn")))))

(deftest get-rbac-group-round-trips-params-and-response
  (let [[result params]
        (support/exercise-single! :rbac-groups
                          "com.anthropic.services.blocking.beta.organization.RbacGroupService"
                          "com.anthropic.models.beta.organization.rbacgroups.BetaRbacGroup"
                          #(anthropic.organization/get-rbac-group % "grp_42"))]
    (is (= "grp_42" (.orElse (.rbacGroupId params) nil)))
    (is (= "stub" (:id result)))
    (is (= :stub (:source-type result)))))

(deftest dynamic-static-call-picks-overload-by-argument-type
  ;; String/valueOf has eight one-arg overloads; getMethods order is unspecified,
  ;; so picking by arity alone returns a different one from run to run.
  (let [call #'anthropic.organization/dynamic-static-call]
    (is (= "ab" (call String "valueOf" (char-array "ab"))))
    (is (= "true" (call String "valueOf" true)))
    (is (re-find #"^java.lang.Object@" (call String "valueOf" (Object.))))))
