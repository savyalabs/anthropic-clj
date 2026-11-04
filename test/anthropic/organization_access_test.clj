(ns anthropic.organization-access-test
  (:require [anthropic.organization :as organization]
            [anthropic.organization-test-support :as support]
            [clojure.test :refer [deftest is]])
  (:import (com.anthropic.core JsonValue)
           (com.anthropic.models.beta AnthropicBeta)
           (com.anthropic.models.organization.rbacgroups RbacGroup
                                                         RbacGroup$SourceType
                                                         DeletedRbacGroup)
           (com.anthropic.models.organization.rbacroles RbacRole)
           (com.anthropic.models.beta.organization.spendlimits BetaSpendLimit
                                                               BetaSpendLimit$Scope
                                                               BetaSpendLimitOrganizationScope
                                                               BetaSpendLimitPeriod
                                                               BetaSpendLimitUsageTierScope
                                                               SpendLimitListParams$ScopeType
                                                               SpendLimitDeleteResponse)
           (java.time OffsetDateTime)))

(def ^:private timestamp (OffsetDateTime/parse "2026-09-30T12:34:56Z"))

(defn- rbac-group []
  (-> (RbacGroup/builder)
      (.id "grp_engineering")
      (.createdAt timestamp)
      (.name "Engineering")
      (.roleIds ["role_admin" "role_developer"])
      (.sourceType (RbacGroup$SourceType/of "direct"))
      (.type (JsonValue/from "rbac_group"))
      (.updatedAt timestamp)
      (.build)))

(defn- rbac-role []
  (-> (RbacRole/builder)
      (.id "role_developer")
      (.createdAt timestamp)
      (.displayName "Developer role")
      (.putAdditionalProperty "name" (JsonValue/from "Developer"))
      (.type (JsonValue/from "rbac_role"))
      (.updatedAt timestamp)
      (.build)))

(defn- organization-scope []
  (-> (BetaSpendLimitOrganizationScope/builder)
      (.type (JsonValue/from "organization"))
      (.build)))

(defn- spend-limit []
  (-> (BetaSpendLimit/builder)
      (.id "sl_monthly_org")
      (.amount "1250.00")
      (.createdAt timestamp)
      (.currency "USD")
      (.isEnabled true)
      (.period (BetaSpendLimitPeriod/of "monthly"))
      (.scope (BetaSpendLimit$Scope/ofOrganization (organization-scope)))
      (.type (JsonValue/from "spend_limit"))
      (.updatedAt timestamp)
      (.build)))

(defn- exercise-single! [root interface response invoke]
  (let [captured (atom nil)
        service (support/leaf-service interface response captured)
        path (cond
               (re-find #"rbacgroups\.MemberService" interface)
               ["com.anthropic.services.blocking.organization.RbacGroupService"]
               (re-find #"spendlimits\.(Effective|IncreaseRequest)Service" interface)
               ["com.anthropic.services.blocking.organization.SpendLimitService"]
               :else [])]
    [(invoke (support/beta-client {root (support/path-service path service)})) @captured]))

(defn- exercise-list! [root interfaces leaf-interface page-class response-class item invoke]
  (let [captured (atom nil)
        page-service (support/proxy-service (Class/forName leaf-interface) (fn [_ _] nil))
        service (support/leaf-service
                 leaf-interface
                 (fn [[params]]
                   (support/list-page page-class response-class page-service params [item item]))
                 captured)
        root-service (support/path-service interfaces service)]
    [(invoke (support/beta-client {root root-service})) @captured]))

(defn- present [optional]
  (.orElse optional nil))

(defn- missing-key [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo error
         (select-keys (ex-data error) [:anthropic/error :key]))))

(defn- rbac-group-member []
  (-> (support/sdk-model
       "com.anthropic.models.organization.rbacgroups.members.RbacGroupMember")
      (.toBuilder)
      (.createdAt timestamp)
      (.email "ada@example.com")
      (.rbacGroupId "grp_engineering")
      (.type (JsonValue/from "rbac_group_member"))
      (.userId "user_ada")
      (.build)))

(defn- deleted-rbac-group-member []
  (-> (support/sdk-model
       "com.anthropic.models.organization.rbacgroups.members.DeletedRbacGroupMember")
      (.toBuilder)
      (.rbacGroupId "grp_engineering")
      (.type (JsonValue/from "rbac_group_member_deleted"))
      (.userId "user_ada")
      (.build)))

(defn- rbac-role-permission []
  (-> (support/sdk-model
       "com.anthropic.models.organization.rbacroles.permissions.RbacRolePermission")
      (.toBuilder)
      (.action "read")
      (.organizationResource "organization")
      (.type (JsonValue/from "rbac_role_permission"))
      (.build)))

(defn- spend-summary []
  (-> (support/sdk-model "com.anthropic.models.organization.spendlimits.SpendSummary")
      (.toBuilder)
      (.amount "1250.00")
      (.currency "USD")
      (.periodToDateSpend "42.50")
      (.spendLimitId "sl_monthly_org")
      (.build)))

(defn- spend-limit-increase-request []
  (-> (support/sdk-model
       "com.anthropic.models.organization.spendlimits.increaserequests.SpendLimitIncreaseRequest")
      (.toBuilder)
      (.id "slir_42")
      (.createdAt timestamp)
      (.spendSummary (spend-summary))
      (.type (JsonValue/from "spend_limit_increase_request"))
      (.build)))

(defn- approved-spend-limit-increase-request []
  (-> (support/sdk-model
       "com.anthropic.models.organization.spendlimits.increaserequests.IncreaseRequestApproveResponse")
      (.toBuilder)
      (.id "slir_42")
      (.createdAt timestamp)
      (.spendSummary (spend-summary))
      (.type (JsonValue/from "spend_limit_increase_request"))
      (.build)))

(defn- routing-client [services calls]
  (let [stable (support/proxy-service
                (Class/forName "com.anthropic.services.blocking.OrganizationService")
                (fn [method _]
                  (swap! calls conj [:stable method])
                  (get services method)))
        beta-organization (support/proxy-service
                           (Class/forName "com.anthropic.services.blocking.beta.OrganizationService")
                           (fn [method _]
                             (swap! calls conj [:beta method])
                             (throw (ex-info "beta organization service was called" {:method method}))))]
    (reify com.anthropic.client.AnthropicClient
      (organization [_] stable)
      (beta [_]
        (reify com.anthropic.services.blocking.BetaService
          (organization [_] beta-organization))))))

(deftest get-spend-limit-round-trips
  (let [[result params]
        (exercise-single! :spend-limits
                          "com.anthropic.services.blocking.beta.organization.SpendLimitService"
                          (spend-limit)
                          #(organization/get-spend-limit % "sl_monthly_org"))]
    (is (= "sl_monthly_org" (.orElse (.spendLimitId params) nil)))
    (is (= #{:id :amount :created-at :currency :is-enabled :period :scope :type :updated-at}
           (set (keys result))))
    (is (= "sl_monthly_org" (:id result)))
    (is (= "1250.00" (:amount result)))
    (is (= "USD" (:currency result)))
    (is (= true (:is-enabled result)))
    (is (= :monthly (:period result)))
    (is (= {:type :organization} (:scope result)))
    (is (= "spend_limit" (:type result)))
    (is (= "2026-09-30T12:34:56Z" (:created-at result)))
    (is (= "2026-09-30T12:34:56Z" (:updated-at result)))))

(deftest delete-spend-limit-round-trips
  (let [response (-> (SpendLimitDeleteResponse/builder)
                     (.id "sl_monthly_org")
                     (.type (JsonValue/from "spend_limit_deleted"))
                     (.build))
        [result params]
        (exercise-single! :spend-limits
                          "com.anthropic.services.blocking.beta.organization.SpendLimitService"
                          response
                          #(organization/delete-spend-limit % "sl_monthly_org"))]
    (is (= "sl_monthly_org" (.orElse (.spendLimitId params) nil)))
    (is (= {:id "sl_monthly_org" :type "spend_limit_deleted"} result))))

(deftest set-spend-limit-round-trips
  (let [scope (organization-scope)
        [result params]
        (exercise-single! :spend-limits
                          "com.anthropic.services.blocking.beta.organization.SpendLimitService"
                          (spend-limit)
                          #(organization/set-spend-limit
                            % {:amount "1250.00" :scope scope :period :monthly
                               :betas [:spend-limit-reads-2026-09-26]}))]
    (is (= "1250.00" (.orElse (.amount params) nil)))
    (is (.isOrganization (.scope params)))
    (is (= "organization"
           (str (.. (.scope params) asOrganization _type))))
    (is (= (BetaSpendLimitPeriod/of "monthly")
           (.orElse (.period params) nil)))
    (is (= [(AnthropicBeta/of "spend-limit-reads-2026-09-26")]
           (.orElse (.betas params) nil)))
    (is (= #{:id :amount :created-at :currency :is-enabled :period :scope :type :updated-at}
           (set (keys result))))
    (is (= "1250.00" (:amount result)))
    (is (= true (:is-enabled result)))
    (is (= :monthly (:period result)))
    (is (= {:type :organization} (:scope result)))))

(deftest list-spend-limits-round-trips
  (let [[result params]
        (exercise-list! :spend-limits []
                        "com.anthropic.services.blocking.beta.organization.SpendLimitService"
                        "com.anthropic.models.beta.organization.spendlimits.SpendLimitListPage"
                        "com.anthropic.models.beta.organization.spendlimits.SpendLimitListPageResponse"
                        (spend-limit)
                        #(organization/list-spend-limits
                          % {:limit 25
                             :page "page_2"
                             :scope-type [:organization "workspace"]
                             :betas [:spend-limit-reads-2026-09-26]}))]
    (is (= 25 (.orElse (.limit params) nil)))
    (is (= "page_2" (.orElse (.page params) nil)))
    (is (= [(SpendLimitListParams$ScopeType/of "organization")
            (SpendLimitListParams$ScopeType/of "workspace")]
           (.orElse (.scopeType params) nil)))
    (is (= [(AnthropicBeta/of "spend-limit-reads-2026-09-26")]
           (.orElse (.betas params) nil)))
    (is (= ["sl_monthly_org" "sl_monthly_org"] (mapv :id result)))))

(deftest create-rbac-group-round-trips
  (let [[result params]
        (exercise-single! :rbac-groups
                          "com.anthropic.services.blocking.organization.RbacGroupService"
                          (rbac-group)
                          #(organization/create-rbac-group % {:name "Engineering"}))]
    (is (= "Engineering" (.name params)))
    (is (= #{:id :created-at :name :role-ids :source-type :type :updated-at}
           (set (keys result))))
    (is (= "grp_engineering" (:id result)))
    (is (= "Engineering" (:name result)))
    (is (= ["role_admin" "role_developer"] (:role-ids result)))
    (is (= :direct (:source-type result)))
    (is (= "rbac_group" (:type result)))
    (is (= "2026-09-30T12:34:56Z" (:created-at result)))
    (is (= "2026-09-30T12:34:56Z" (:updated-at result)))))

(deftest get-rbac-group-round-trips
  (let [[result params]
        (exercise-single! :rbac-groups
                          "com.anthropic.services.blocking.organization.RbacGroupService"
                          (rbac-group)
                          #(organization/get-rbac-group % "grp_engineering"))]
    (is (= "grp_engineering" (.orElse (.rbacGroupId params) nil)))
    (is (= #{:id :created-at :name :role-ids :source-type :type :updated-at}
           (set (keys result))))
    (is (= "grp_engineering" (:id result)))
    (is (= "Engineering" (:name result)))
    (is (= ["role_admin" "role_developer"] (:role-ids result)))
    (is (= :direct (:source-type result)))
    (is (= "rbac_group" (:type result)))
    (is (= "2026-09-30T12:34:56Z" (:created-at result)))
    (is (= "2026-09-30T12:34:56Z" (:updated-at result)))))

(deftest update-rbac-group-round-trips
  (let [[result params]
        (exercise-single! :rbac-groups
                          "com.anthropic.services.blocking.organization.RbacGroupService"
                          (rbac-group)
                          #(organization/update-rbac-group
                            % "grp_engineering" {:name "Engineering"}))]
    (is (= "grp_engineering" (.orElse (.rbacGroupId params) nil)))
    (is (= "Engineering" (.orElse (.name params) nil)))
    (is (= #{:id :created-at :name :role-ids :source-type :type :updated-at}
           (set (keys result))))
    (is (= "grp_engineering" (:id result)))
    (is (= "Engineering" (:name result)))
    (is (= ["role_admin" "role_developer"] (:role-ids result)))
    (is (= :direct (:source-type result)))
    (is (= "rbac_group" (:type result)))
    (is (= "2026-09-30T12:34:56Z" (:created-at result)))
    (is (= "2026-09-30T12:34:56Z" (:updated-at result)))))

(deftest list-rbac-groups-round-trips
  (let [[result params]
        (exercise-list! :rbac-groups []
                        "com.anthropic.services.blocking.organization.RbacGroupService"
                        "com.anthropic.models.organization.rbacgroups.RbacGroupListPage"
                        "com.anthropic.models.organization.rbacgroups.RbacGroupListPageResponse"
                        (rbac-group)
                        #(organization/list-rbac-groups % {:limit 25 :page "page_2"}))
        first-group (first result)]
    (is (= 25 (.orElse (.limit params) nil)))
    (is (= "page_2" (.orElse (.page params) nil)))
    (is (= 2 (count result)))
    (is (= #{:id :created-at :name :role-ids :source-type :type :updated-at}
           (set (keys first-group))))
    (is (= "grp_engineering" (:id first-group)))
    (is (= "Engineering" (:name first-group)))
    (is (= ["role_admin" "role_developer"] (:role-ids first-group)))
    (is (= :direct (:source-type first-group)))
    (is (= "rbac_group" (:type first-group)))
    (is (= "2026-09-30T12:34:56Z" (:created-at first-group)))
    (is (= "2026-09-30T12:34:56Z" (:updated-at first-group)))))

(deftest delete-rbac-group-round-trips
  (let [response (-> (DeletedRbacGroup/builder)
                     (.id "grp_engineering")
                     (.type (JsonValue/from "rbac_group_deleted"))
                     (.build))
        [result params]
        (exercise-single! :rbac-groups
                          "com.anthropic.services.blocking.organization.RbacGroupService"
                          response
                          #(organization/delete-rbac-group % "grp_engineering"))]
    (is (= "grp_engineering" (.orElse (.rbacGroupId params) nil)))
    (is (= {:id "grp_engineering" :type "rbac_group_deleted"} result))))

(deftest get-rbac-role-round-trips
  (let [[result params]
        (exercise-single! :rbac-roles
                          "com.anthropic.services.blocking.organization.RbacRoleService"
                          (rbac-role)
                          #(organization/get-rbac-role % "role_developer"))]
    (is (= "role_developer" (.orElse (.rbacRoleId params) nil)))
    (is (= #{:id :created-at :display-name :name :type :updated-at} (set (keys result))))
    (is (= "role_developer" (:id result)))
    (is (= "Developer" (:name result)))
    (is (= "Developer role" (:display-name result)))
    (is (= "rbac_role" (:type result)))
    (is (= "2026-09-30T12:34:56Z" (:created-at result)))
    (is (= "2026-09-30T12:34:56Z" (:updated-at result)))))

(deftest list-rbac-roles-round-trips
  (let [[result params]
        (exercise-list! :rbac-roles []
                        "com.anthropic.services.blocking.organization.RbacRoleService"
                        "com.anthropic.models.organization.rbacroles.RbacRoleListPage"
                        "com.anthropic.models.organization.rbacroles.RbacRoleListPageResponse"
                        (rbac-role)
                        #(organization/list-rbac-roles % {:limit 25 :page "page_2"}))
        first-role (first result)]
    (is (= 25 (.orElse (.limit params) nil)))
    (is (= "page_2" (.orElse (.page params) nil)))
    (is (= 2 (count result)))
    (is (= #{:id :created-at :display-name :name :type :updated-at} (set (keys first-role))))
    (is (= "role_developer" (:id first-role)))
    (is (= "Developer" (:name first-role)))
    (is (= "Developer role" (:display-name first-role)))
    (is (= "rbac_role" (:type first-role)))
    (is (= "2026-09-30T12:34:56Z" (:created-at first-role)))
    (is (= "2026-09-30T12:34:56Z" (:updated-at first-role)))))

(deftest delete-rbac-group-uses-stable-service
  (let [calls (atom [])
        captured (atom nil)
        service (support/leaf-service
                 "com.anthropic.services.blocking.organization.RbacGroupService"
                 (-> (DeletedRbacGroup/builder)
                     (.id "grp_engineering")
                     (.type (JsonValue/from "rbac_group_deleted"))
                     (.build))
                 captured)]
    (organization/delete-rbac-group (routing-client {"rbacGroups" service} calls) "grp_engineering")
    (is (= [[:stable "rbacGroups"]] @calls))
    (is (= "grp_engineering" (present (.rbacGroupId @captured))))))

(deftest list-rbac-groups-uses-stable-service
  (let [calls (atom [])
        captured (atom nil)
        page-service (support/proxy-service
                      (Class/forName "com.anthropic.services.blocking.organization.RbacGroupService")
                      (fn [_ _] nil))
        service (support/leaf-service
                 "com.anthropic.services.blocking.organization.RbacGroupService"
                 (fn [[params]]
                   (support/list-page
                    "com.anthropic.models.organization.rbacgroups.RbacGroupListPage"
                    "com.anthropic.models.organization.rbacgroups.RbacGroupListPageResponse"
                    page-service params [(rbac-group)]))
                 captured)]
    (organization/list-rbac-groups (routing-client {"rbacGroups" service} calls))
    (is (= [[:stable "rbacGroups"]] @calls))))

(deftest get-rbac-role-uses-stable-service
  (let [calls (atom [])
        captured (atom nil)
        service (support/leaf-service
                 "com.anthropic.services.blocking.organization.RbacRoleService"
                 (rbac-role) captured)]
    (organization/get-rbac-role (routing-client {"rbacRoles" service} calls) "role_developer")
    (is (= [[:stable "rbacRoles"]] @calls))
    (is (= "role_developer" (present (.rbacRoleId @captured))))))

(deftest list-rbac-roles-uses-stable-service
  (let [calls (atom [])
        captured (atom nil)
        page-service (support/proxy-service
                      (Class/forName "com.anthropic.services.blocking.organization.RbacRoleService")
                      (fn [_ _] nil))
        service (support/leaf-service
                 "com.anthropic.services.blocking.organization.RbacRoleService"
                 (fn [[params]]
                   (support/list-page
                    "com.anthropic.models.organization.rbacroles.RbacRoleListPage"
                    "com.anthropic.models.organization.rbacroles.RbacRoleListPageResponse"
                    page-service params [(rbac-role)]))
                 captured)]
    (organization/list-rbac-roles (routing-client {"rbacRoles" service} calls))
    (is (= [[:stable "rbacRoles"]] @calls))))

(deftest list-spend-limits-builds-workspace-and-usage-tier-params
  (let [[_ params]
        (exercise-list! :spend-limits []
                        "com.anthropic.services.blocking.beta.organization.SpendLimitService"
                        "com.anthropic.models.beta.organization.spendlimits.SpendLimitListPage"
                        "com.anthropic.models.beta.organization.spendlimits.SpendLimitListPageResponse"
                        (spend-limit)
                        #(organization/list-spend-limits
                          % {:workspace-id "wrkspc_1" :scope-type [:usage-tier]}))]
    (is (= "wrkspc_1" (.orElse (.workspaceId params) nil)))
    (is (= [SpendLimitListParams$ScopeType/USAGE_TIER] (.orElse (.scopeType params) nil)))))

(deftest spend-limit-usage-tier-scope-round-trips
  (let [usage-tier (-> (spend-limit)
                       (.toBuilder)
                       (.scope (BetaSpendLimit$Scope/ofUsageTier
                                (-> (BetaSpendLimitUsageTierScope/builder)
                                    (.type (JsonValue/from "usage_tier"))
                                    (.build))))
                       (.build))
        [result] (exercise-single! :spend-limits
                                   "com.anthropic.services.blocking.beta.organization.SpendLimitService"
                                   usage-tier
                                   #(organization/get-spend-limit % "sl_monthly_org"))]
    (is (= {:type :usage-tier} (:scope result)))))

(deftest set-spend-limit-builds-scope-from-map
  (let [[_ params]
        (exercise-single! :spend-limits
                          "com.anthropic.services.blocking.beta.organization.SpendLimitService"
                          (spend-limit)
                          #(organization/set-spend-limit
                            % {:amount "50.00" :period :monthly
                               :scope {:type :workspace :workspace-id "wrkspc_1"}}))]
    (is (.isWorkspace (.scope params)))
    (is (= "wrkspc_1" (.workspaceId (.asWorkspace (.scope params)))))))

(deftest add-rbac-group-member-builds-params
  (let [[_ params] (exercise-single! :rbac-groups
                                     "com.anthropic.services.blocking.organization.rbacgroups.MemberService"
                                     (rbac-group-member)
                                     #(organization/add-rbac-group-member % "grp_engineering" {:user-id "user_ada"}))]
    (is (= "grp_engineering" (present (.rbacGroupId params))))
    (is (= "user_ada" (.userId params)))))

(deftest add-rbac-group-member-requires-user-id
  (is (= {:anthropic/error :missing-key :key :user-id}
         (missing-key #(organization/add-rbac-group-member (support/beta-client {}) "grp_engineering" {}))))
  (is (= {:anthropic/error :missing-key :key :rbac-group-id}
         (missing-key #(organization/add-rbac-group-member (support/beta-client {}) nil {:user-id "user_ada"})))))

(deftest add-rbac-group-member-round-trips
  (let [[result] (exercise-single! :rbac-groups
                                   "com.anthropic.services.blocking.organization.rbacgroups.MemberService"
                                   (rbac-group-member)
                                   #(organization/add-rbac-group-member % "grp_engineering" {:user-id "user_ada"}))]
    (is (= {:created-at "2026-09-30T12:34:56Z" :email "ada@example.com"
            :rbac-group-id "grp_engineering" :type "rbac_group_member" :user-id "user_ada"}
           result))))

(deftest list-rbac-group-members-builds-params
  (let [[_ params] (exercise-list! :rbac-groups
                                   ["com.anthropic.services.blocking.organization.RbacGroupService"]
                                   "com.anthropic.services.blocking.organization.rbacgroups.MemberService"
                                   "com.anthropic.models.organization.rbacgroups.members.MemberListPage"
                                   "com.anthropic.models.organization.rbacgroups.members.MemberListPageResponse"
                                   (rbac-group-member)
                                   #(organization/list-rbac-group-members % "grp_engineering" {:limit 25 :page "members_2"}))]
    (is (= "grp_engineering" (present (.rbacGroupId params))))
    (is (= 25 (present (.limit params))))
    (is (= "members_2" (present (.page params))))))

(deftest list-rbac-group-members-requires-rbac-group-id
  (is (= {:anthropic/error :missing-key :key :rbac-group-id}
         (missing-key #(organization/list-rbac-group-members (support/beta-client {}) nil)))))

(deftest list-rbac-group-members-round-trips
  (let [[result] (exercise-list! :rbac-groups
                                 ["com.anthropic.services.blocking.organization.RbacGroupService"]
                                 "com.anthropic.services.blocking.organization.rbacgroups.MemberService"
                                 "com.anthropic.models.organization.rbacgroups.members.MemberListPage"
                                 "com.anthropic.models.organization.rbacgroups.members.MemberListPageResponse"
                                 (rbac-group-member)
                                 #(organization/list-rbac-group-members % "grp_engineering"))]
    (is (= ["user_ada" "user_ada"] (mapv :user-id result)))
    (is (= "ada@example.com" (:email (first result))))))

(deftest remove-rbac-group-member-builds-params
  (let [[_ params] (exercise-single! :rbac-groups
                                     "com.anthropic.services.blocking.organization.rbacgroups.MemberService"
                                     (deleted-rbac-group-member)
                                     #(organization/remove-rbac-group-member % "grp_engineering" "user_ada"))]
    (is (= "grp_engineering" (.rbacGroupId params)))
    (is (= "user_ada" (present (.userId params))))))

(deftest remove-rbac-group-member-requires-user-id
  (is (= {:anthropic/error :missing-key :key :user-id}
         (missing-key #(organization/remove-rbac-group-member (support/beta-client {}) "grp_engineering" nil))))
  (is (= {:anthropic/error :missing-key :key :rbac-group-id}
         (missing-key #(organization/remove-rbac-group-member (support/beta-client {}) nil "user_ada")))))

(deftest remove-rbac-group-member-round-trips
  (let [[result] (exercise-single! :rbac-groups
                                   "com.anthropic.services.blocking.organization.rbacgroups.MemberService"
                                   (deleted-rbac-group-member)
                                   #(organization/remove-rbac-group-member % "grp_engineering" "user_ada"))]
    (is (= {:rbac-group-id "grp_engineering" :type "rbac_group_member_deleted" :user-id "user_ada"}
           result))))

(deftest list-rbac-role-permissions-builds-params
  (let [[_ params] (exercise-list! :rbac-roles
                                   ["com.anthropic.services.blocking.organization.RbacRoleService"]
                                   "com.anthropic.services.blocking.organization.rbacroles.PermissionService"
                                   "com.anthropic.models.organization.rbacroles.permissions.PermissionListPage"
                                   "com.anthropic.models.organization.rbacroles.permissions.PermissionListPageResponse"
                                   (rbac-role-permission)
                                   #(organization/list-rbac-role-permissions % "role_developer" {:limit 25 :page "permissions_2"}))]
    (is (= "role_developer" (present (.rbacRoleId params))))
    (is (= 25 (present (.limit params))))
    (is (= "permissions_2" (present (.page params))))))

(deftest list-rbac-role-permissions-requires-rbac-role-id
  (is (= {:anthropic/error :missing-key :key :rbac-role-id}
         (missing-key #(organization/list-rbac-role-permissions (support/beta-client {}) nil)))))

(deftest list-rbac-role-permissions-round-trips
  (let [[result] (exercise-list! :rbac-roles
                                 ["com.anthropic.services.blocking.organization.RbacRoleService"]
                                 "com.anthropic.services.blocking.organization.rbacroles.PermissionService"
                                 "com.anthropic.models.organization.rbacroles.permissions.PermissionListPage"
                                 "com.anthropic.models.organization.rbacroles.permissions.PermissionListPageResponse"
                                 (rbac-role-permission)
                                 #(organization/list-rbac-role-permissions % "role_developer"))]
    (is (= ["read" "read"] (mapv :action result)))
    (is (= :organization (get-in (first result) [:resource :type])))))

(deftest list-effective-spend-limits-builds-params
  (let [[_ params] (exercise-list! :spend-limits
                                   ["com.anthropic.services.blocking.organization.SpendLimitService"]
                                   "com.anthropic.services.blocking.organization.spendlimits.EffectiveService"
                                   "com.anthropic.models.organization.spendlimits.effective.EffectiveListPage"
                                   "com.anthropic.models.organization.spendlimits.effective.EffectiveListPageResponse"
                                   (spend-summary)
                                   #(organization/list-effective-spend-limits % {:limit 25 :page "effective_2" :period [:monthly] :user-ids ["user_ada"]}))]
    (is (= 25 (present (.limit params))))
    (is (= "effective_2" (present (.page params))))
    (is (= ["monthly"] (mapv #(.asString %) (present (.period params)))))
    (is (= ["user_ada"] (present (.userIds params))))))

(deftest list-effective-spend-limits-round-trips
  (let [[result] (exercise-list! :spend-limits
                                 ["com.anthropic.services.blocking.organization.SpendLimitService"]
                                 "com.anthropic.services.blocking.organization.spendlimits.EffectiveService"
                                 "com.anthropic.models.organization.spendlimits.effective.EffectiveListPage"
                                 "com.anthropic.models.organization.spendlimits.effective.EffectiveListPageResponse"
                                 (spend-summary)
                                 #(organization/list-effective-spend-limits %))]
    (is (= ["sl_monthly_org" "sl_monthly_org"] (mapv :spend-limit-id result)))
    (is (= "42.50" (:period-to-date-spend (first result))))))

(deftest list-spend-limit-increase-requests-builds-params
  (let [[_ params] (exercise-list! :spend-limits
                                   ["com.anthropic.services.blocking.organization.SpendLimitService"]
                                   "com.anthropic.services.blocking.organization.spendlimits.IncreaseRequestService"
                                   "com.anthropic.models.organization.spendlimits.increaserequests.IncreaseRequestListPage"
                                   "com.anthropic.models.organization.spendlimits.increaserequests.IncreaseRequestListPageResponse"
                                   (spend-limit-increase-request)
                                   #(organization/list-spend-limit-increase-requests % {:actor-ids ["user_ada"] :limit 25 :page "requests_2" :status [:pending]}))]
    (is (= ["user_ada"] (present (.actorIds params))))
    (is (= 25 (present (.limit params))))
    (is (= "requests_2" (present (.page params))))
    (is (= ["pending"] (mapv #(.asString %) (present (.status params)))))))

(deftest list-spend-limit-increase-requests-round-trips
  (let [[result] (exercise-list! :spend-limits
                                 ["com.anthropic.services.blocking.organization.SpendLimitService"]
                                 "com.anthropic.services.blocking.organization.spendlimits.IncreaseRequestService"
                                 "com.anthropic.models.organization.spendlimits.increaserequests.IncreaseRequestListPage"
                                 "com.anthropic.models.organization.spendlimits.increaserequests.IncreaseRequestListPageResponse"
                                 (spend-limit-increase-request)
                                 #(organization/list-spend-limit-increase-requests %))]
    (is (= ["slir_42" "slir_42"] (mapv :id result)))
    (is (= "1250.00" (get-in (first result) [:spend-summary :amount])))))

(deftest get-spend-limit-increase-request-builds-params
  (let [[_ params] (exercise-single! :spend-limits
                                     "com.anthropic.services.blocking.organization.spendlimits.IncreaseRequestService"
                                     (spend-limit-increase-request)
                                     #(organization/get-spend-limit-increase-request % "slir_42"))]
    (is (= "slir_42" (present (.spendLimitIncreaseRequestId params))))))

(deftest get-spend-limit-increase-request-requires-id
  (is (= {:anthropic/error :missing-key :key :spend-limit-increase-request-id}
         (missing-key #(organization/get-spend-limit-increase-request (support/beta-client {}) nil)))))

(deftest get-spend-limit-increase-request-round-trips
  (let [[result] (exercise-single! :spend-limits
                                   "com.anthropic.services.blocking.organization.spendlimits.IncreaseRequestService"
                                   (spend-limit-increase-request)
                                   #(organization/get-spend-limit-increase-request % "slir_42"))]
    (is (= "slir_42" (:id result)))
    (is (= "1250.00" (get-in result [:spend-summary :amount])))))

(deftest approve-spend-limit-increase-request-builds-params
  (let [[_ params] (exercise-single! :spend-limits
                                     "com.anthropic.services.blocking.organization.spendlimits.IncreaseRequestService"
                                     (approved-spend-limit-increase-request)
                                     #(organization/approve-spend-limit-increase-request % "slir_42" {:amount "2500.00" :period :monthly :suppress-notification true}))]
    (is (= "slir_42" (present (.spendLimitIncreaseRequestId params))))
    (is (= "2500.00" (.amount params)))
    (is (= "monthly" (.asString (present (.period params)))))
    (is (= true (present (.suppressNotification params))))))

(deftest approve-spend-limit-increase-request-requires-amount
  (is (= {:anthropic/error :missing-key :key :amount}
         (missing-key #(organization/approve-spend-limit-increase-request (support/beta-client {}) "slir_42" {}))))
  (is (= {:anthropic/error :missing-key :key :spend-limit-increase-request-id}
         (missing-key #(organization/approve-spend-limit-increase-request (support/beta-client {}) nil {:amount "2500.00"})))))

(deftest approve-spend-limit-increase-request-round-trips
  (let [[result] (exercise-single! :spend-limits
                                   "com.anthropic.services.blocking.organization.spendlimits.IncreaseRequestService"
                                   (approved-spend-limit-increase-request)
                                   #(organization/approve-spend-limit-increase-request % "slir_42" {:amount "2500.00"}))]
    (is (= "slir_42" (:id result)))
    (is (= "1250.00" (get-in result [:spend-summary :amount])))))

(deftest deny-spend-limit-increase-request-builds-params
  (let [[_ params] (exercise-single! :spend-limits
                                     "com.anthropic.services.blocking.organization.spendlimits.IncreaseRequestService"
                                     (spend-limit-increase-request)
                                     #(organization/deny-spend-limit-increase-request % "slir_42" {:suppress-notification true}))]
    (is (= "slir_42" (present (.spendLimitIncreaseRequestId params))))
    (is (= true (present (.suppressNotification params))))))

(deftest deny-spend-limit-increase-request-requires-id
  (is (= {:anthropic/error :missing-key :key :spend-limit-increase-request-id}
         (missing-key #(organization/deny-spend-limit-increase-request (support/beta-client {}) nil)))))

(deftest deny-spend-limit-increase-request-round-trips
  (let [[result] (exercise-single! :spend-limits
                                   "com.anthropic.services.blocking.organization.spendlimits.IncreaseRequestService"
                                   (spend-limit-increase-request)
                                   #(organization/deny-spend-limit-increase-request % "slir_42"))]
    (is (= "slir_42" (:id result)))
    (is (= "1250.00" (get-in result [:spend-summary :amount])))))
