(ns anthropic.organization-claude-code-managed-settings-test
  (:require [anthropic.organization :as organization]
            [anthropic.organization-test-support :as support]
            [clojure.test :refer [deftest is]]))

(def ^:private claude-code "com.anthropic.services.blocking.beta.organization.ClaudeCodeService")
(def ^:private managed-settings "com.anthropic.services.blocking.beta.organization.claudecode.ManagedSettingService")
(def ^:private configuration "com.anthropic.services.blocking.beta.organization.claudecode.managedsettings.ConfigurationService")
(def ^:private rbac-group-default "com.anthropic.services.blocking.beta.organization.claudecode.managedsettings.RbacGroupDefaultService")
(def ^:private rbac-groups "com.anthropic.services.blocking.beta.organization.claudecode.managedsettings.RbacGroupService")

(defn- present [optional] (.orElse optional nil))

(defn- configuration-response []
  (support/from-json
   "com.anthropic.models.beta.organization.claudecode.managedsettings.configuration.BetaClaudeCodeManagedSettingsConfiguration"
   {"type" "claude_code_managed_settings_configuration"
    "mode" {"type" "organization"}
    "mode_updated_at" "2026-11-22T00:00:00Z"
    "rbac_group_order" ["grp_1"]
    "rbac_group_order_version" "version_1"}))

(defn- default-response []
  (support/from-json
   "com.anthropic.models.beta.organization.claudecode.managedsettings.rbacgroupdefault.BetaClaudeCodeRbacGroupDefaultManagedSettings"
   {"type" "claude_code_rbac_group_default_managed_settings"
    "settings" {"enabled" true}
    "updated_at" "2026-11-22T00:00:00Z"}))

(defn- group-response []
  (support/from-json
   "com.anthropic.models.beta.organization.claudecode.managedsettings.rbacgroups.BetaClaudeCodeRbacGroupManagedSettings"
   {"type" "claude_code_rbac_group_managed_settings"
    "rbac_group_id" "grp_1"
    "rbac_group_status" {"type" "active"}
    "settings" {"enabled" true}
    "created_at" "2026-11-22T00:00:00Z"
    "updated_at" "2026-11-22T00:00:00Z"}))

(defn- deleted-group-response []
  (support/from-json
   "com.anthropic.models.beta.organization.claudecode.managedsettings.rbacgroups.BetaDeletedClaudeCodeRbacGroupManagedSettings"
   {"type" "deleted_claude_code_rbac_group_managed_settings"
    "id" "grp_1"}))

(deftest managed-settings-configuration-retrieve-round-trips
  (let [[result params]
        (support/exercise-path! :claude-code [claude-code managed-settings] configuration
                                (configuration-response)
                                #(organization/get-claude-code-managed-settings-configuration %))]
    (is (some? params))
    (is (= :organization (get-in result [:mode :type])))
    (is (= ["grp_1"] (:rbac-group-order result)))))

(deftest managed-settings-configuration-update-builds-params
  (let [[result params]
        (support/exercise-path! :claude-code [claude-code managed-settings] configuration
                                (configuration-response)
                                #(organization/update-claude-code-managed-settings-configuration
                                  % {:mode {:type :rbac-groups}
                                     :rbac-group-order ["grp_1"]
                                     :rbac-group-order-version "version_1"
                                     :betas [:ce-claude-code-managed-settings-2026-10-06]}))]
    (is (.isRbacGroups (present (.mode params))))
    (is (= ["grp_1"] (present (.rbacGroupOrder params))))
    (is (= "version_1" (present (.rbacGroupOrderVersion params))))
    (is (= ["ce-claude-code-managed-settings-2026-10-06"]
           (mapv #(.asString %) (present (.betas params)))))
    (is (= :organization (get-in result [:mode :type])))))

(deftest managed-settings-rbac-group-default-retrieve-round-trips
  (let [[result params]
        (support/exercise-path! :claude-code [claude-code managed-settings] rbac-group-default
                                (default-response)
                                #(organization/get-claude-code-rbac-group-default-managed-settings %))]
    (is (some? params))
    (is (= true (get-in result [:settings :enabled])))))

(deftest managed-settings-rbac-group-default-update-builds-params
  (let [[result params]
        (support/exercise-path! :claude-code [claude-code managed-settings] rbac-group-default
                                (default-response)
                                #(organization/update-claude-code-rbac-group-default-managed-settings
                                  % {:settings {:enabled true}}))]
    (is (some? (.settings params)))
    (is (= true (get-in result [:settings :enabled])))))

(deftest managed-settings-rbac-groups-list-builds-params
  (let [[result params]
        (support/exercise-path-list! :claude-code [claude-code managed-settings] rbac-groups
                                     "com.anthropic.models.beta.organization.claudecode.managedsettings.rbacgroups.RbacGroupListPage"
                                     "com.anthropic.models.beta.organization.claudecode.managedsettings.rbacgroups.RbacGroupListPageResponse"
                                     [(group-response)]
                                     #(organization/list-claude-code-rbac-group-managed-settings
                                       % {:limit 10 :page "next"}))]
    (is (= 10 (present (.limit params))))
    (is (= "next" (present (.page params))))
    (is (= ["grp_1"] (mapv :rbac-group-id result)))))

(deftest managed-settings-rbac-group-retrieve-builds-params
  (let [[result params]
        (support/exercise-path! :claude-code [claude-code managed-settings] rbac-groups
                                (group-response)
                                #(organization/get-claude-code-rbac-group-managed-settings % "grp_1"))]
    (is (= "grp_1" (present (.rbacGroupId params))))
    (is (= :active (get-in result [:rbac-group-status :type])))))

(deftest managed-settings-rbac-group-set-builds-params
  (let [[result params]
        (support/exercise-path! :claude-code [claude-code managed-settings] rbac-groups
                                (group-response)
                                #(organization/set-claude-code-rbac-group-managed-settings
                                  % "grp_1" {:settings {:enabled true}}))]
    (is (= "grp_1" (present (.rbacGroupId params))))
    (is (some? (.settings params)))
    (is (= "grp_1" (:rbac-group-id result)))))

(deftest managed-settings-rbac-group-delete-builds-params
  (let [[result params]
        (support/exercise-path! :claude-code [claude-code managed-settings] rbac-groups
                                (deleted-group-response)
                                #(organization/delete-claude-code-rbac-group-managed-settings % "grp_1"))]
    (is (= "grp_1" (present (.rbacGroupId params))))
    (is (= "grp_1" (:id result)))))

(deftest managed-settings-rbac-group-operations-require-an-id
  (doseq [invoke [#(organization/get-claude-code-rbac-group-managed-settings % nil)
                  #(organization/set-claude-code-rbac-group-managed-settings % nil {:settings {}})
                  #(organization/delete-claude-code-rbac-group-managed-settings % nil)]]
    (let [error (try (invoke (support/beta-client {}))
                     nil
                     (catch clojure.lang.ExceptionInfo e e))]
      (is (= {:anthropic/error :missing-key :key :rbac-group-id}
             (select-keys (ex-data error) [:anthropic/error :key]))))))
