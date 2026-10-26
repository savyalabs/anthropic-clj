(ns anthropic.organization-analytics-test
  (:require [anthropic.organization :as organization]
            [anthropic.organization-test-support :as support]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(def ^:private analytics-service
  "com.anthropic.services.blocking.beta.organization.AnalyticsService")

(defn- present [optional]
  (.orElse optional nil))

(defn- enum-values [optional]
  (mapv #(.asString %) (present optional)))

(defn- kebab-keyword? [key]
  (and (keyword? key)
       (not (str/includes? (name key) "_"))))

(defn- kebab-keys? [value]
  (cond
    (map? value) (and (every? kebab-keyword? (keys value))
                      (every? kebab-keys? (vals value)))
    (sequential? value) (every? kebab-keys? value)
    :else true))

(defn- assert-list-result! [result expected-keys value-key expected-value enum-keys]
  (let [item (first result)]
    (is (= 2 (count result)))
    (is (= expected-keys (set (keys item))))
    (is (kebab-keys? item))
    (doseq [key enum-keys]
      (is (keyword? (get item key))))
    (is (= expected-value (get item value-key)))))

(deftest analytics-chat-cowork-unified-fields-convert
  (let [summary
        (-> (support/sdk-model
             "com.anthropic.models.beta.organization.analytics.BetaAnalyticsSingleDayActivitySummary")
            (.toBuilder)
            (.chatCoworkUnifiedDailyActiveUserCount 11)
            (.chatCoworkUnifiedMonthlyActiveUserCount 22)
            (.chatCoworkUnifiedWeeklyActiveUserCount 33)
            (.build))
        chat (-> (support/sdk-model
                  "com.anthropic.models.beta.organization.analytics.BetaAnalyticsChatCoworkUnifiedChatMetrics")
                 (.toBuilder)
                 (.messageCount 44)
                 (.thinkingMessageCount 55)
                 (.build))
        unified (-> (support/sdk-model
                     "com.anthropic.models.beta.organization.analytics.BetaAnalyticsUserActivity$ChatCoworkUnifiedMetrics")
                    (.toBuilder)
                    (.chat chat)
                    (.build))
        result (-> (support/sdk-model
                    "com.anthropic.models.beta.organization.analytics.BetaAnalyticsUserActivity")
                   (.toBuilder)
                   (.chatCoworkUnifiedMetrics unified)
                   (.build))
        summary-map (#'organization/obj->clj summary)
        result-map (#'organization/obj->clj result)]
    (is (= 11 (:chat-cowork-unified-daily-active-user-count summary-map)))
    (is (= 22 (:chat-cowork-unified-monthly-active-user-count summary-map)))
    (is (= 33 (:chat-cowork-unified-weekly-active-user-count summary-map)))
    (is (= 44 (get-in result-map [:chat-cowork-unified-metrics :chat :message-count])))
    (is (= 55 (get-in result-map [:chat-cowork-unified-metrics :chat :thinking-message-count])))))

(deftest list-analytics-summaries-round-trips
  (let [[result params]
        (support/exercise-list!
         :analytics [analytics-service]
         "com.anthropic.services.blocking.beta.organization.analytics.SummaryService"
         "com.anthropic.models.beta.organization.analytics.summaries.SummaryListPage"
         "com.anthropic.models.beta.organization.analytics.summaries.SummaryListPageResponse"
         "com.anthropic.models.beta.organization.analytics.BetaAnalyticsSingleDayActivitySummary"
         #(organization/list-analytics-summaries
           % {:starting-date "2026-09-01"
              :ending-date "2026-09-30"
              :filter ["product:claude_code"]
              :limit 25
              :page "summary-page"}))]
    (is (= "2026-09-01" (str (.startingDate params))))
    (is (= "2026-09-30" (str (present (.endingDate params)))))
    (is (= ["product:claude_code"] (vec (present (.filter params)))))
    (is (= 25 (present (.limit params))))
    (is (= "summary-page" (present (.page params))))
    (assert-list-result!
     result
     #{:assigned-seat-count :cowork-daily-active-user-count
       :cowork-monthly-active-user-count :cowork-weekly-active-user-count
       :daily-active-user-count :daily-adoption-rate :ending-at
       :monthly-active-user-count :monthly-adoption-rate :pending-invite-count
       :starting-at :weekly-active-user-count :weekly-adoption-rate}
     :daily-active-user-count 1 [])))

(deftest list-analytics-users-round-trips
  (let [[result params]
        (support/exercise-list!
         :analytics [analytics-service]
         "com.anthropic.services.blocking.beta.organization.analytics.UserService"
         "com.anthropic.models.beta.organization.analytics.users.UserListPage"
         "com.anthropic.models.beta.organization.analytics.users.UserListPageResponse"
         "com.anthropic.models.beta.organization.analytics.BetaAnalyticsUserActivity"
         #(organization/list-analytics-users
           % {:date "2026-09-15"
              :ending-date "2026-09-30"
              :filter ["user_id:usr_42"]
              :group-by [:rbac-group-id]
              :limit 25
              :order :desc
              :order-by "web_search_count"
              :page "user-page"
              :starting-date "2026-09-01"}))]
    (is (= "2026-09-15" (str (present (.date params)))))
    (is (= "2026-09-30" (str (present (.endingDate params)))))
    (is (= ["user_id:usr_42"] (vec (present (.filter params)))))
    (is (= ["rbac_group_id"] (enum-values (.groupBy params))))
    (is (= 25 (present (.limit params))))
    (is (= "desc" (.asString (present (.order params)))))
    (is (= "web_search_count" (present (.orderBy params))))
    (is (= "user-page" (present (.page params))))
    (is (= "2026-09-01" (str (present (.startingDate params)))))
    (assert-list-result!
     result
     #{:chat-metrics :claude-code-metrics :cowork-metrics :design-metrics
       :office-metrics :science-metrics :web-search-count}
     :web-search-count 1 [])))

(deftest list-analytics-chat-projects-round-trips
  (let [[result params]
        (support/exercise-list!
         :analytics [analytics-service
                     "com.anthropic.services.blocking.beta.organization.analytics.AppService"
                     "com.anthropic.services.blocking.beta.organization.analytics.apps.ChatService"]
         "com.anthropic.services.blocking.beta.organization.analytics.apps.chat.ProjectService"
         "com.anthropic.models.beta.organization.analytics.apps.chat.projects.ProjectListPage"
         "com.anthropic.models.beta.organization.analytics.apps.chat.projects.ProjectListPageResponse"
         "com.anthropic.models.beta.organization.analytics.BetaAnalyticsProjectActivity"
         #(organization/list-analytics-chat-projects
           % {:date "2026-09-15"
              :ending-date "2026-09-30"
              :filter ["project_id:prj_42"]
              :group-by [:user-id]
              :limit 25
              :order :asc
              :order-by "message_count"
              :page "project-page"
              :starting-date "2026-09-01"}))]
    (is (= "2026-09-15" (str (present (.date params)))))
    (is (= "2026-09-30" (str (present (.endingDate params)))))
    (is (= ["project_id:prj_42"] (vec (present (.filter params)))))
    (is (= ["user_id"] (enum-values (.groupBy params))))
    (is (= 25 (present (.limit params))))
    (is (= "asc" (.asString (present (.order params)))))
    (is (= "message_count" (present (.orderBy params))))
    (is (= "project-page" (present (.page params))))
    (is (= "2026-09-01" (str (present (.startingDate params)))))
    (assert-list-result!
     result #{:distinct-user-count :message-count :project-id :project-name}
     :project-id "stub" [])))

(deftest list-analytics-connectors-round-trips
  (let [[result params]
        (support/exercise-list!
         :analytics [analytics-service]
         "com.anthropic.services.blocking.beta.organization.analytics.ConnectorService"
         "com.anthropic.models.beta.organization.analytics.connectors.ConnectorListPage"
         "com.anthropic.models.beta.organization.analytics.connectors.ConnectorListPageResponse"
         "com.anthropic.models.beta.organization.analytics.BetaAnalyticsConnectorActivity"
         #(organization/list-analytics-connectors
           % {:date "2026-09-15"
              :ending-date "2026-09-30"
              :filter ["connector_name:drive"]
              :group-by [:product]
              :limit 25
              :order :desc
              :order-by "distinct_user_count"
              :page "connector-page"
              :starting-date "2026-09-01"}))]
    (is (= "2026-09-15" (str (present (.date params)))))
    (is (= "2026-09-30" (str (present (.endingDate params)))))
    (is (= ["connector_name:drive"] (vec (present (.filter params)))))
    (is (= ["product"] (enum-values (.groupBy params))))
    (is (= 25 (present (.limit params))))
    (is (= "desc" (.asString (present (.order params)))))
    (is (= "distinct_user_count" (present (.orderBy params))))
    (is (= "connector-page" (present (.page params))))
    (is (= "2026-09-01" (str (present (.startingDate params)))))
    (assert-list-result!
     result
     #{:chat-metrics :claude-code-metrics :connector-name :cowork-metrics
       :distinct-user-count :office-metrics}
     :connector-name "stub" [])))

(deftest list-analytics-plugins-round-trips
  (let [[result params]
        (support/exercise-list!
         :analytics [analytics-service]
         "com.anthropic.services.blocking.beta.organization.analytics.PluginService"
         "com.anthropic.models.beta.organization.analytics.plugins.PluginListPage"
         "com.anthropic.models.beta.organization.analytics.plugins.PluginListPageResponse"
         "com.anthropic.models.beta.organization.analytics.BetaAnalyticsPluginActivity"
         #(organization/list-analytics-plugins
           % {:date "2026-09-15"
              :ending-date "2026-09-30"
              :filter ["plugin_name:reviewer"]
              :group-by [:product]
              :limit 25
              :order :asc
              :order-by "install_count"
              :page "plugin-page"
              :starting-date "2026-09-01"}))]
    (is (= "2026-09-15" (str (present (.date params)))))
    (is (= "2026-09-30" (str (present (.endingDate params)))))
    (is (= ["plugin_name:reviewer"] (vec (present (.filter params)))))
    (is (= ["product"] (enum-values (.groupBy params))))
    (is (= 25 (present (.limit params))))
    (is (= "asc" (.asString (present (.order params)))))
    (is (= "install_count" (present (.orderBy params))))
    (is (= "plugin-page" (present (.page params))))
    (is (= "2026-09-01" (str (present (.startingDate params)))))
    (assert-list-result!
     result
     #{:claude-code-metrics :cowork-metrics :distinct-user-count :install-count
       :invocation-count :plugin-name}
     :plugin-name "stub" [])))

(deftest list-analytics-skills-round-trips
  (let [[result params]
        (support/exercise-list!
         :analytics [analytics-service]
         "com.anthropic.services.blocking.beta.organization.analytics.SkillService"
         "com.anthropic.models.beta.organization.analytics.skills.SkillListPage"
         "com.anthropic.models.beta.organization.analytics.skills.SkillListPageResponse"
         "com.anthropic.models.beta.organization.analytics.BetaAnalyticsSkillActivity"
         #(organization/list-analytics-skills
           % {:date "2026-09-15"
              :ending-date "2026-09-30"
              :filter ["skill_name:spreadsheet"]
              :group-by [:product]
              :limit 25
              :order :desc
              :order-by "distinct_user_count"
              :page "skill-page"
              :starting-date "2026-09-01"}))]
    (is (= "2026-09-15" (str (present (.date params)))))
    (is (= "2026-09-30" (str (present (.endingDate params)))))
    (is (= ["skill_name:spreadsheet"] (vec (present (.filter params)))))
    (is (= ["product"] (enum-values (.groupBy params))))
    (is (= 25 (present (.limit params))))
    (is (= "desc" (.asString (present (.order params)))))
    (is (= "distinct_user_count" (present (.orderBy params))))
    (is (= "skill-page" (present (.page params))))
    (is (= "2026-09-01" (str (present (.startingDate params)))))
    (assert-list-result!
     result
     #{:chat-metrics :claude-code-metrics :cowork-metrics :distinct-user-count
       :office-metrics :skill-name}
     :skill-name "stub" [])))

(deftest list-analytics-artifacts-round-trips
  (let [[result params]
        (support/exercise-list!
         :analytics [analytics-service]
         "com.anthropic.services.blocking.beta.organization.analytics.ArtifactService"
         "com.anthropic.models.beta.organization.analytics.artifacts.ArtifactListPage"
         "com.anthropic.models.beta.organization.analytics.artifacts.ArtifactListPageResponse"
         "com.anthropic.models.beta.organization.analytics.BetaAnalyticsArtifactActivity"
         #(organization/list-analytics-artifacts
           % {:date "2026-09-15"
              :filter ["artifact_type:spreadsheet"]
              :group-by [:product]
              :limit 25
              :page "artifact-page"}))]
    (is (= "2026-09-15" (str (.date params))))
    (is (= ["artifact_type:spreadsheet"] (vec (present (.filter params)))))
    (is (= ["product"] (enum-values (.groupBy params))))
    (is (= 25 (present (.limit params))))
    (is (= "artifact-page" (present (.page params))))
    (assert-list-result!
     result
     #{:artifact-type :artifacts-created-count :distinct-user-count :is-shared
       :published-artifacts-created-count}
     :artifacts-created-count 1 [])))

(deftest list-analytics-usage-report-round-trips
  (let [[result params]
        (support/exercise-list!
         :analytics [analytics-service]
         "com.anthropic.services.blocking.beta.organization.analytics.UsageReportService"
         "com.anthropic.models.beta.organization.analytics.usagereport.UsageReportListPage"
         "com.anthropic.models.beta.organization.analytics.usagereport.UsageReportListPageResponse"
         "com.anthropic.models.beta.organization.analytics.BetaAnalyticsUsageBucketedResult"
         #(organization/list-analytics-usage-report
           % {:starting-at "2026-09-01T00:00:00Z"
              :bucket-width :day
              :claude-tag-categories [:engaged]
              :claude-tag-user-ids ["usr_tag_42"]
              :context-windows [:from-0-to-200k]
              :ending-at "2026-09-30T00:00:00Z"
              :group-by [:model]
              :inference-geos [:us]
              :limit 25
              :models ["claude-sonnet-4-5"]
              :page "usage-page"
              :products [:claude-code]
              :rbac-group-ids ["grp_42"]
              :slack-channel-ids ["C042"]
              :speeds [:fast]
              :user-ids ["usr_42"]}))]
    (is (= "2026-09-01T00:00Z" (str (.startingAt params))))
    (is (= "1d" (.asString (present (.bucketWidth params)))))
    (is (= ["engaged"] (enum-values (.claudeTagCategories params))))
    (is (= ["usr_tag_42"] (vec (present (.claudeTagUserIds params)))))
    (is (= ["0-200k"] (enum-values (.contextWindows params))))
    (is (= "2026-09-30T00:00Z" (str (present (.endingAt params)))))
    (is (= ["model"] (enum-values (.groupBy params))))
    (is (= ["us"] (enum-values (.inferenceGeos params))))
    (is (= 25 (present (.limit params))))
    (is (= ["claude-sonnet-4-5"] (vec (present (.models params)))))
    (is (= "usage-page" (present (.page params))))
    (is (= ["claude_code"] (enum-values (.products params))))
    (is (= ["grp_42"] (vec (present (.rbacGroupIds params)))))
    (is (= ["C042"] (vec (present (.slackChannelIds params)))))
    (is (= ["fast"] (enum-values (.speeds params))))
    (is (= ["usr_42"] (vec (present (.userIds params)))))
    (assert-list-result!
     result
     #{:cache-creation :cache-read-input-tokens :claude-tag-category
       :claude-tag-user-id :context-window :inference-geo :model :output-tokens
       :product :rbac-group-id :requests :server-tool-use :slack-channel-id
       :speed :uncached-input-tokens}
     :requests 1 [:claude-tag-category :context-window :inference-geo :speed])))

(deftest list-analytics-user-usage-report-round-trips
  (let [[result params]
        (support/exercise-list!
         :analytics [analytics-service]
         "com.anthropic.services.blocking.beta.organization.analytics.UserUsageReportService"
         "com.anthropic.models.beta.organization.analytics.userusagereport.UserUsageReportListPage"
         "com.anthropic.models.beta.organization.analytics.userusagereport.UserUsageReportListPageResponse"
         "com.anthropic.models.beta.organization.analytics.BetaAnalyticsUsageUsersItem"
         #(organization/list-analytics-user-usage-report
           % {:starting-at "2026-09-01T00:00:00Z"
              :bucket-width :day
              :claude-tag-categories [:engaged]
              :claude-tag-user-ids ["usr_tag_42"]
              :context-windows [:from-0-to-200k]
              :ending-at "2026-09-30T00:00:00Z"
              :exclude-deleted-users true
              :group-by [:model]
              :inference-geos [:us]
              :limit 25
              :models ["claude-sonnet-4-5"]
              :order :desc
              :order-by :total-tokens
              :page "user-usage-page"
              :products [:claude-code]
              :rbac-group-ids ["grp_42"]
              :slack-channel-ids ["C042"]
              :speeds [:fast]
              :user-ids ["usr_42"]}))]
    (is (= "2026-09-01T00:00Z" (str (.startingAt params))))
    (is (= "1d" (.asString (present (.bucketWidth params)))))
    (is (= ["engaged"] (enum-values (.claudeTagCategories params))))
    (is (= ["usr_tag_42"] (vec (present (.claudeTagUserIds params)))))
    (is (= ["0-200k"] (enum-values (.contextWindows params))))
    (is (= "2026-09-30T00:00Z" (str (present (.endingAt params)))))
    (is (= true (present (.excludeDeletedUsers params))))
    (is (= ["model"] (enum-values (.groupBy params))))
    (is (= ["us"] (enum-values (.inferenceGeos params))))
    (is (= 25 (present (.limit params))))
    (is (= ["claude-sonnet-4-5"] (vec (present (.models params)))))
    (is (= "desc" (.asString (present (.order params)))))
    (is (= "total_tokens" (.asString (present (.orderBy params)))))
    (is (= "user-usage-page" (present (.page params))))
    (is (= ["claude_code"] (enum-values (.products params))))
    (is (= ["grp_42"] (vec (present (.rbacGroupIds params)))))
    (is (= ["C042"] (vec (present (.slackChannelIds params)))))
    (is (= ["fast"] (enum-values (.speeds params))))
    (is (= ["usr_42"] (vec (present (.userIds params)))))
    (assert-list-result!
     result
     #{:actor :cache-creation :cache-read-input-tokens :claude-tag-category
       :claude-tag-user-id :context-window :ending-at :inference-geo :model
       :output-tokens :product :rbac-group-id :requests :server-tool-use
       :slack-channel-id :speed :starting-at :total-tokens
       :uncached-input-tokens}
     :total-tokens 1 [:claude-tag-category :context-window :inference-geo :speed])))

(deftest list-analytics-cost-report-round-trips
  (let [[result params]
        (support/exercise-list!
         :analytics [analytics-service]
         "com.anthropic.services.blocking.beta.organization.analytics.CostReportService"
         "com.anthropic.models.beta.organization.analytics.costreport.CostReportListPage"
         "com.anthropic.models.beta.organization.analytics.costreport.CostReportListPageResponse"
         "com.anthropic.models.beta.organization.analytics.BetaAnalyticsCostBucketedResult"
         #(organization/list-analytics-cost-report
           % {:starting-at "2026-09-01T00:00:00Z"
              :bucket-width :day
              :claude-tag-categories [:engaged]
              :claude-tag-user-ids ["usr_tag_42"]
              :context-windows [:from-0-to-200k]
              :ending-at "2026-09-30T00:00:00Z"
              :group-by [:cost-type]
              :inference-geos [:us]
              :limit 25
              :models ["claude-sonnet-4-5"]
              :page "cost-page"
              :products [:claude-code]
              :rbac-group-ids ["grp_42"]
              :slack-channel-ids ["C042"]
              :speeds [:fast]
              :user-ids ["usr_42"]}))]
    (is (= "2026-09-01T00:00Z" (str (.startingAt params))))
    (is (= "1d" (.asString (present (.bucketWidth params)))))
    (is (= ["engaged"] (enum-values (.claudeTagCategories params))))
    (is (= ["usr_tag_42"] (vec (present (.claudeTagUserIds params)))))
    (is (= ["0-200k"] (enum-values (.contextWindows params))))
    (is (= "2026-09-30T00:00Z" (str (present (.endingAt params)))))
    (is (= ["cost_type"] (enum-values (.groupBy params))))
    (is (= ["us"] (enum-values (.inferenceGeos params))))
    (is (= 25 (present (.limit params))))
    (is (= ["claude-sonnet-4-5"] (vec (present (.models params)))))
    (is (= "cost-page" (present (.page params))))
    (is (= ["claude_code"] (enum-values (.products params))))
    (is (= ["grp_42"] (vec (present (.rbacGroupIds params)))))
    (is (= ["C042"] (vec (present (.slackChannelIds params)))))
    (is (= ["fast"] (enum-values (.speeds params))))
    (is (= ["usr_42"] (vec (present (.userIds params)))))
    (assert-list-result!
     result
     #{:amount :claude-tag-category
       :claude-tag-user-id :context-window :cost-type :currency :inference-geo
       :list-amount :model :product :rbac-group-id :requests :slack-channel-id
       :speed :token-type}
     :requests 1 [:claude-tag-category :context-window :cost-type :inference-geo
                  :speed :token-type])))

(deftest list-analytics-user-cost-report-round-trips
  (let [[result params]
        (support/exercise-list!
         :analytics [analytics-service]
         "com.anthropic.services.blocking.beta.organization.analytics.UserCostReportService"
         "com.anthropic.models.beta.organization.analytics.usercostreport.UserCostReportListPage"
         "com.anthropic.models.beta.organization.analytics.usercostreport.UserCostReportListPageResponse"
         "com.anthropic.models.beta.organization.analytics.BetaAnalyticsCostUsersItem"
         #(organization/list-analytics-user-cost-report
           % {:starting-at "2026-09-01T00:00:00Z"
              :bucket-width :day
              :claude-tag-categories [:engaged]
              :claude-tag-user-ids ["usr_tag_42"]
              :context-windows [:from-0-to-200k]
              :ending-at "2026-09-30T00:00:00Z"
              :exclude-deleted-users true
              :group-by [:cost-type]
              :inference-geos [:us]
              :limit 25
              :models ["claude-sonnet-4-5"]
              :order :desc
              :order-by :requests
              :page "user-cost-page"
              :products [:claude-code]
              :rbac-group-ids ["grp_42"]
              :slack-channel-ids ["C042"]
              :speeds [:fast]
              :user-ids ["usr_42"]}))]
    (is (= "2026-09-01T00:00Z" (str (.startingAt params))))
    (is (= "1d" (.asString (present (.bucketWidth params)))))
    (is (= ["engaged"] (enum-values (.claudeTagCategories params))))
    (is (= ["usr_tag_42"] (vec (present (.claudeTagUserIds params)))))
    (is (= ["0-200k"] (enum-values (.contextWindows params))))
    (is (= "2026-09-30T00:00Z" (str (present (.endingAt params)))))
    (is (= true (present (.excludeDeletedUsers params))))
    (is (= ["cost_type"] (enum-values (.groupBy params))))
    (is (= ["us"] (enum-values (.inferenceGeos params))))
    (is (= 25 (present (.limit params))))
    (is (= ["claude-sonnet-4-5"] (vec (present (.models params)))))
    (is (= "desc" (.asString (present (.order params)))))
    (is (= "requests" (.asString (present (.orderBy params)))))
    (is (= "user-cost-page" (present (.page params))))
    (is (= ["claude_code"] (enum-values (.products params))))
    (is (= ["grp_42"] (vec (present (.rbacGroupIds params)))))
    (is (= ["C042"] (vec (present (.slackChannelIds params)))))
    (is (= ["fast"] (enum-values (.speeds params))))
    (is (= ["usr_42"] (vec (present (.userIds params)))))
    (assert-list-result!
     result
     #{:actor :amount :claude-tag-category :claude-tag-user-id :context-window
       :cost-type :currency :ending-at :inference-geo :list-amount :model
       :product :rbac-group-id :requests :slack-channel-id :speed :starting-at
       :token-type}
     :requests 1 [:claude-tag-category :context-window :cost-type :inference-geo
                  :speed :token-type])))

(deftest list-analytics-usage-builds-params
  (let [[result params]
        (support/exercise-list!
         :analytics [analytics-service]
         "com.anthropic.services.blocking.beta.organization.analytics.UsageService"
         "com.anthropic.models.beta.organization.analytics.usage.UsageListPage"
         "com.anthropic.models.beta.organization.analytics.usage.UsageListPageResponse"
         "com.anthropic.models.beta.organization.analytics.usage.BetaAnalyticsUsageItem"
         #(organization/list-analytics-usage
           % {:starting-at "2026-10-01T00:00:00Z"
              :ending-at "2026-10-08T00:00:00Z"
              :time-bucket-width :day
              :actor-types [:user-actor :service-account-actor]
              :actor-ids ["user_1"]
              :group-by [:model :workspace-id]
              :products [:claude-code :messages-api]
              :workspace-ids ["wrkspc_1"]
              :exclude-deleted-users true
              :order :desc
              :order-by :total-tokens
              :limit 50
              :page "usage-page"}))]
    (is (= "2026-10-01T00:00Z" (str (.startingAt params))))
    (is (= "2026-10-08T00:00Z" (str (present (.endingAt params)))))
    (is (= "1d" (.asString (present (.timeBucketWidth params)))))
    (is (= ["user_actor" "service_account_actor"] (enum-values (.actorTypes params))))
    (is (= ["user_1"] (vec (present (.actorIds params)))))
    (is (= ["model" "workspace_id"] (enum-values (.groupBy params))))
    (is (= ["claude_code" "messages_api"] (enum-values (.products params))))
    (is (= ["wrkspc_1"] (vec (present (.workspaceIds params)))))
    (is (= true (present (.excludeDeletedUsers params))))
    (is (= "desc" (.asString (present (.order params)))))
    (is (= "total_tokens" (.asString (present (.orderBy params)))))
    (is (= 50 (present (.limit params))))
    (is (= "usage-page" (present (.page params))))
    (is (= 2 (count result)))
    (is (kebab-keys? (first result)))))

(deftest list-analytics-usage-requires-starting-at
  (let [error (try (organization/list-analytics-usage (support/beta-client {}) {})
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
    (is (= {:anthropic/error :missing-key :key :starting-at}
           (select-keys (ex-data error) [:anthropic/error :key])))))

(deftest list-analytics-usage-round-trips
  (let [item (support/from-json
              "com.anthropic.models.beta.organization.analytics.usage.BetaAnalyticsUsageItem"
              {"starting_at" "2026-10-01T00:00:00Z"
               "ending_at" "2026-10-02T00:00:00Z"
               "group" {"model" "claude-opus-5-5" "workspace_id" "wrkspc_1"}
               "metrics" {"requests" 12 "output_tokens" 300}})
        [result]
        (support/exercise-path-list!
         :analytics [analytics-service]
         "com.anthropic.services.blocking.beta.organization.analytics.UsageService"
         "com.anthropic.models.beta.organization.analytics.usage.UsageListPage"
         "com.anthropic.models.beta.organization.analytics.usage.UsageListPageResponse"
         [item]
         #(organization/list-analytics-usage % {:starting-at "2026-10-01T00:00:00Z"}))]
    (is (= [{:starting-at "2026-10-01T00:00:00Z"
             :ending-at "2026-10-02T00:00:00Z"
             :group {:model "claude-opus-5-5" :workspace-id "wrkspc_1"}
             :metrics {:requests 12 :output-tokens 300}}]
           result))))

(deftest list-analytics-cost-builds-params
  (let [[result params]
        (support/exercise-list!
         :analytics [analytics-service]
         "com.anthropic.services.blocking.beta.organization.analytics.CostService"
         "com.anthropic.models.beta.organization.analytics.cost.CostListPage"
         "com.anthropic.models.beta.organization.analytics.cost.CostListPageResponse"
         "com.anthropic.models.beta.organization.analytics.cost.BetaAnalyticsCostItem"
         #(organization/list-analytics-cost
           % {:starting-at "2026-10-01T00:00:00Z"
              :time-bucket-width :hour
              :group-by [:cost-type :token-type]
              :order-by :list-amount
              :order :asc
              :api-key-ids ["apikey_1"]}))]
    (is (= "2026-10-01T00:00Z" (str (.startingAt params))))
    (is (= "1h" (.asString (present (.timeBucketWidth params)))))
    (is (= ["cost_type" "token_type"] (enum-values (.groupBy params))))
    (is (= "list_amount" (.asString (present (.orderBy params)))))
    (is (= "asc" (.asString (present (.order params)))))
    (is (= ["apikey_1"] (vec (present (.apiKeyIds params)))))
    (is (= 2 (count result)))))

(deftest list-analytics-cost-requires-starting-at
  (let [error (try (organization/list-analytics-cost (support/beta-client {}) {})
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
    (is (= {:anthropic/error :missing-key :key :starting-at}
           (select-keys (ex-data error) [:anthropic/error :key])))))
