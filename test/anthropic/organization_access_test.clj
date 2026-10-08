(ns anthropic.organization-access-test
  (:require [anthropic.organization :as organization]
            [anthropic.organization-test-support :as support]
            [clojure.test :refer [deftest is]])
  (:import (com.anthropic.core JsonValue)
           (com.anthropic.models.beta AnthropicBeta)
           (com.anthropic.models.beta.organization.rbacgroups BetaRbacGroup
                                                              BetaRbacGroup$SourceType
                                                              RbacGroupDeleteResponse)
           (com.anthropic.models.beta.organization.rbacroles BetaRbacRole)
           (com.anthropic.models.beta.organization.spendlimits BetaSpendLimit
                                                               BetaSpendLimit$Scope
                                                               BetaSpendLimitOrganizationScope
                                                               BetaSpendLimitPeriod
                                                               SpendLimitListParams$ScopeType
                                                               SpendLimitDeleteResponse)
           (java.time OffsetDateTime)))

(def ^:private timestamp (OffsetDateTime/parse "2026-09-30T12:34:56Z"))

(defn- rbac-group []
  (-> (BetaRbacGroup/builder)
      (.id "grp_engineering")
      (.createdAt timestamp)
      (.name "Engineering")
      (.roleIds ["role_admin" "role_developer"])
      (.sourceType (BetaRbacGroup$SourceType/of "direct"))
      (.type (JsonValue/from "rbac_group"))
      (.updatedAt timestamp)
      (.build)))

(defn- rbac-role []
  (-> (BetaRbacRole/builder)
      (.id "role_developer")
      (.createdAt timestamp)
      (.displayName "Developer role")
      (.name "Developer")
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
        service (support/leaf-service interface response captured)]
    [(invoke (support/beta-client {root service})) @captured]))

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
                          "com.anthropic.services.blocking.beta.organization.RbacGroupService"
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
                          "com.anthropic.services.blocking.beta.organization.RbacGroupService"
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
                          "com.anthropic.services.blocking.beta.organization.RbacGroupService"
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
                        "com.anthropic.services.blocking.beta.organization.RbacGroupService"
                        "com.anthropic.models.beta.organization.rbacgroups.RbacGroupListPage"
                        "com.anthropic.models.beta.organization.rbacgroups.RbacGroupListPageResponse"
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
  (let [response (-> (RbacGroupDeleteResponse/builder)
                     (.id "grp_engineering")
                     (.type (JsonValue/from "rbac_group_deleted"))
                     (.build))
        [result params]
        (exercise-single! :rbac-groups
                          "com.anthropic.services.blocking.beta.organization.RbacGroupService"
                          response
                          #(organization/delete-rbac-group % "grp_engineering"))]
    (is (= "grp_engineering" (.orElse (.rbacGroupId params) nil)))
    (is (= {:id "grp_engineering" :type "rbac_group_deleted"} result))))

(deftest get-rbac-role-round-trips
  (let [[result params]
        (exercise-single! :rbac-roles
                          "com.anthropic.services.blocking.beta.organization.RbacRoleService"
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
                        "com.anthropic.services.blocking.beta.organization.RbacRoleService"
                        "com.anthropic.models.beta.organization.rbacroles.RbacRoleListPage"
                        "com.anthropic.models.beta.organization.rbacroles.RbacRoleListPageResponse"
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
