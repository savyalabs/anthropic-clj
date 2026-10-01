(ns anthropic.organization-plugins-test
  (:require [anthropic.organization :as organization]
            [anthropic.organization-test-support :as support]
            [clojure.test :refer [deftest is]])
  (:import (java.io ByteArrayInputStream)
           (java.nio.charset StandardCharsets)
           (java.time OffsetDateTime)))

(def ^:private now (OffsetDateTime/parse "2026-09-30T12:00:00Z"))

(defn- plugin []
  (-> (com.anthropic.models.beta.organization.plugins.BetaPlugin/builder)
      (.id "plg_weather")
      (.components [])
      (.contentScan
       (-> (com.anthropic.models.beta.organization.plugins.BetaPluginContentScan/builder)
           (.assessment
            (com.anthropic.models.beta.organization.plugins.BetaPluginContentScan$Assessment/of "pass"))
           (.status
            (com.anthropic.models.beta.organization.plugins.BetaPluginContentScan$Status/of "completed"))
           (.reason "No issues found")
           (.build)))
      (.createdAt now)
      (.apiActorCreatedBy "apikey_admin")
      (.description "Shows the local forecast")
      (.displayName "Weather")
      (.latestVersionId "plgv_2")
      (.manifestVersion "1.2.0")
      (.marketplaceId "pm_weather")
      (.name "weather")
      (.organizationInstallationPreference
       (com.anthropic.models.beta.organization.plugins.BetaPlugin$OrganizationInstallationPreference/of
        "available"))
      (.organizationInstallationPreferenceInherited false)
      (.userOwner "usr_ada")
      (.reach (com.anthropic.models.beta.organization.plugins.BetaPlugin$Reach/of "remote"))
      (.servedVersionId "plgv_2")
      (.servedVersionPinned true)
      (.updatedAt now)
      (.build)))

(defn- installation-setting []
  (-> (com.anthropic.models.beta.organization.plugins.installationsettings.BetaPluginInstallationSetting/builder)
      (.createdAt now)
      (.installationPreference
       (com.anthropic.models.beta.organization.plugins.installationsettings.BetaPluginInstallationSetting$InstallationPreference/of
        "required"))
      (.pluginId "plg_weather")
      (.target
       (com.anthropic.models.beta.organization.plugins.installationsettings.BetaPluginInstallationSetting$Target/ofOrganizationMember
        "usr_ada"))
      (.updatedAt now)
      (.build)))

(defn- marketplace []
  (-> (com.anthropic.models.beta.organization.pluginmarketplaces.BetaPluginMarketplace/builder)
      (.id "pm_weather")
      (.createdAt now)
      (.defaultInstallationPreference
       (com.anthropic.models.beta.organization.pluginmarketplaces.BetaPluginMarketplace$DefaultInstallationPreference/of
        "available"))
      (.lastSyncEndedAt now)
      (.lastSyncReadSha "abc123")
      (.name "Weather plugins")
      (.userOwner "usr_ada")
      (.source
       (com.anthropic.models.beta.organization.pluginmarketplaces.BetaPluginMarketplace$Source/of
        "github"))
      (.syncStatus
       (com.anthropic.models.beta.organization.pluginmarketplaces.BetaPluginMarketplace$SyncStatus/of
        "success"))
      (.build)))

(defn- validation-report []
  (-> (com.anthropic.models.beta.organization.pluginmarketplaces.BetaPluginMarketplaceValidationReport/builder)
      (.commitSha "abc123")
      (.manifestError "")
      (.manifestErrorCode "")
      (.pluginErrors [])
      (.pluginWarnings [])
      (.ref "main")
      (.totalPluginCount 2)
      (.valid true)
      (.build)))

(defn- exercise-single-with!
  [root interface response invoke]
  (let [captured (atom nil)
        service (support/leaf-service interface response captured)]
    [(invoke (support/beta-client {root service})) @captured]))

(defn- exercise-list-with!
  [root interfaces leaf-interface page-class response-class items invoke]
  (let [captured (atom nil)
        page-service (support/proxy-service (Class/forName leaf-interface) (fn [_ _] nil))
        service (support/leaf-service
                 leaf-interface
                 (fn [[params]]
                   (support/list-page page-class response-class page-service params items))
                 captured)
        root-service (support/path-service interfaces service)]
    [(invoke (support/beta-client {root root-service})) @captured]))

(defn- exercise-plugin-installation-single-with!
  [response invoke]
  (let [captured (atom nil)
        service (support/leaf-service
                 "com.anthropic.services.blocking.beta.organization.plugins.InstallationSettingService"
                 response captured)
        plugins (support/path-service
                 ["com.anthropic.services.blocking.beta.organization.PluginService"] service)]
    [(invoke (support/beta-client {:plugins plugins})) @captured]))

(deftest create-plugin-round-trips
  (let [archive-a (.getBytes "plugin-a" StandardCharsets/UTF_8)
        archive-b (.getBytes "plugin-b" StandardCharsets/UTF_8)
        [result params]
        (exercise-single-with!
         :plugins
         "com.anthropic.services.blocking.beta.organization.PluginService"
         (plugin)
         #(organization/create-plugin % {:files [(ByteArrayInputStream. archive-a)
                                                 (ByteArrayInputStream. archive-b)]
                                         :marketplace-id "pm_weather"
                                         :release-notes "Add hourly forecasts"}))]
    (is (= ["plugin-a" "plugin-b"]
           (mapv #(String. (.readAllBytes %) StandardCharsets/UTF_8) (.files params))))
    (is (= "pm_weather" (.orElse (.marketplaceId params) nil)))
    (is (= "Add hourly forecasts" (.orElse (.releaseNotes params) nil)))
    (is (= #{:id :created-at :created-by :description :display-name :latest-version-id
             :manifest-version :marketplace-id :name
             :organization-installation-preference
             :content-scan :organization-installation-preference-inherited :owner :reach
             :served-version-id :served-version-pinned :type :updated-at :components}
           (set (keys result))))
    (is (= "plg_weather" (:id result)))
    (is (= "Weather" (:display-name result)))
    (is (= :available (:organization-installation-preference result)))))

(deftest get-plugin-round-trips
  (let [[result params]
        (exercise-single-with!
         :plugins
         "com.anthropic.services.blocking.beta.organization.PluginService"
         (plugin)
         #(organization/get-plugin % "plg_weather"))]
    (is (= "plg_weather" (.orElse (.pluginId params) nil)))
    (is (= #{:id :created-at :created-by :description :display-name :latest-version-id
             :manifest-version :marketplace-id :name
             :organization-installation-preference
             :content-scan :organization-installation-preference-inherited :owner :reach
             :served-version-id :served-version-pinned :type :updated-at :components}
           (set (keys result))))
    (is (= "weather" (:name result)))
    (is (= :available (:organization-installation-preference result)))))

(deftest update-plugin-round-trips
  (let [[result params]
        (exercise-single-with!
         :plugins
         "com.anthropic.services.blocking.beta.organization.PluginService"
         (plugin)
         #(organization/update-plugin % "plg_weather" {:served-version-id "plgv_2"}))]
    (is (= "plg_weather" (.orElse (.pluginId params) nil)))
    (is (= "plgv_2" (.servedVersionId params)))
    (is (= #{:id :created-at :created-by :description :display-name :latest-version-id
             :manifest-version :marketplace-id :name
             :organization-installation-preference
             :content-scan :organization-installation-preference-inherited :owner :reach
             :served-version-id :served-version-pinned :type :updated-at :components}
           (set (keys result))))
    (is (= "plgv_2" (:served-version-id result)))
    (is (= :available (:organization-installation-preference result)))))

(deftest list-plugins-round-trips
  (let [created-gt "2026-01-01T00:00:00Z"
        created-gte "2026-02-01T00:00:00Z"
        created-lt "2026-03-01T00:00:00Z"
        created-lte "2026-04-01T00:00:00Z"
        [result params]
        (exercise-list-with!
         :plugins []
         "com.anthropic.services.blocking.beta.organization.PluginService"
         "com.anthropic.models.beta.organization.plugins.PluginListPage"
         "com.anthropic.models.beta.organization.plugins.PluginListPageResponse"
         [(plugin) (plugin)]
         #(organization/list-plugins % {:created-at-gt created-gt
                                         :created-at-gte created-gte
                                         :created-at-lt created-lt
                                         :created-at-lte created-lte
                                         :limit 25
                                         :marketplace-id "pm_weather"
                                         :organization-id "org_1"
                                         :owner-type :user
                                         :owner-user-id "usr_ada"
                                         :page "page_2"}))]
    (is (= (OffsetDateTime/parse created-gt) (.orElse (.createdAtGt params) nil)))
    (is (= (OffsetDateTime/parse created-gte) (.orElse (.createdAtGte params) nil)))
    (is (= (OffsetDateTime/parse created-lt) (.orElse (.createdAtLt params) nil)))
    (is (= (OffsetDateTime/parse created-lte) (.orElse (.createdAtLte params) nil)))
    (is (= 25 (.orElse (.limit params) nil)))
    (is (= "pm_weather" (.orElse (.marketplaceId params) nil)))
    (is (= "org_1" (.orElse (.organizationId params) nil)))
    (is (= "user" (.asString (.orElse (.ownerType params) nil))))
    (is (= "usr_ada" (.orElse (.ownerUserId params) nil)))
    (is (= "page_2" (.orElse (.page params) nil)))
    (is (= 2 (count result)))
    (is (= #{:id :created-at :created-by :description :display-name :latest-version-id
             :manifest-version :marketplace-id :name
             :organization-installation-preference
             :content-scan :organization-installation-preference-inherited :owner :reach
             :served-version-id :served-version-pinned :type :updated-at :components}
           (set (keys (first result)))))
    (is (= "plg_weather" (:id (first result))))
    (is (= :available (:organization-installation-preference (first result))))))

(deftest delete-plugin-round-trips
  (let [[result params]
        (exercise-single-with!
         :plugins
         "com.anthropic.services.blocking.beta.organization.PluginService"
         (com.anthropic.models.beta.organization.plugins.BetaDeletedPlugin/of "plg_weather")
         #(organization/delete-plugin % "plg_weather"))]
    (is (= "plg_weather" (.orElse (.pluginId params) nil)))
    (is (= {:id "plg_weather" :type "plugin_deleted"} result))))

(deftest list-plugin-installation-settings-round-trips
  (let [[result params]
        (exercise-list-with!
         :plugins ["com.anthropic.services.blocking.beta.organization.PluginService"]
         "com.anthropic.services.blocking.beta.organization.plugins.InstallationSettingService"
         "com.anthropic.models.beta.organization.plugins.installationsettings.InstallationSettingListPage"
         "com.anthropic.models.beta.organization.plugins.installationsettings.InstallationSettingListPageResponse"
         [(installation-setting) (installation-setting)]
         #(organization/list-plugin-installation-settings
           % "plg_weather" {:limit 10 :organization-id "org_1"
                             :page "page_2" :target-type :organization-member}))]
    (is (= "plg_weather" (.orElse (.pluginId params) nil)))
    (is (= 10 (.orElse (.limit params) nil)))
    (is (= "org_1" (.orElse (.organizationId params) nil)))
    (is (= "page_2" (.orElse (.page params) nil)))
    (is (= "organization_member" (.asString (.orElse (.targetType params) nil))))
    (is (= 2 (count result)))
    (is (= #{:created-at :installation-preference :plugin-id :target :type :updated-at}
           (set (keys (first result)))))
    (is (= "plg_weather" (:plugin-id (first result))))
    (is (= :required (:installation-preference (first result))))))

(deftest remove-plugin-installation-setting-round-trips
  (let [[result params]
        (exercise-plugin-installation-single-with!
         (-> (com.anthropic.models.beta.organization.plugins.installationsettings.BetaDeletedPluginInstallationSetting/builder)
             (.pluginId "plg_weather")
             (.target
              (com.anthropic.models.beta.organization.plugins.installationsettings.BetaDeletedPluginInstallationSetting$Target/ofOrganizationMember
               "usr_ada"))
             (.build))
         #(organization/remove-plugin-installation-setting % "plg_weather" "usr_ada"))]
    (is (= "plg_weather" (.pluginId params)))
    (is (= "usr_ada" (.orElse (.target params) nil)))
    (is (= #{:plugin-id :target :type} (set (keys result))))
    (is (= "plg_weather" (:plugin-id result)))))

(deftest set-plugin-installation-setting-round-trips
  (let [[result params]
        (exercise-plugin-installation-single-with!
         (installation-setting)
         #(organization/set-plugin-installation-setting
           % "plg_weather" {:target "usr_ada" :installation-preference :required}))]
    (is (= "plg_weather" (.pluginId params)))
    (is (= "usr_ada" (.orElse (.target params) nil)))
    (is (= "required" (.asString (.installationPreference params))))
    (is (= #{:created-at :installation-preference :plugin-id :target :type :updated-at}
           (set (keys result))))
    (is (= "plg_weather" (:plugin-id result)))
    (is (= :required (:installation-preference result)))))

(deftest get-plugin-marketplace-round-trips
  (let [[result params]
        (exercise-single-with!
         :plugin-marketplaces
         "com.anthropic.services.blocking.beta.organization.PluginMarketplaceService"
         (marketplace)
         #(organization/get-plugin-marketplace % "pm_weather"))]
    (is (= "pm_weather" (.orElse (.marketplaceId params) nil)))
    (is (= #{:id :created-at :default-installation-preference :last-sync-ended-at
             :last-sync-read-sha :name :owner :source :sync-status :type}
           (set (keys result))))
    (is (= "pm_weather" (:id result)))
    (is (= :available (:default-installation-preference result)))
    (is (= :github (:source result)))
    (is (= :success (:sync-status result)))))

(deftest update-plugin-marketplace-round-trips
  (let [[result params]
        (exercise-single-with!
         :plugin-marketplaces
         "com.anthropic.services.blocking.beta.organization.PluginMarketplaceService"
         (marketplace)
         #(organization/update-plugin-marketplace
           % "pm_weather" {:default-installation-preference :available}))]
    (is (= "pm_weather" (.orElse (.marketplaceId params) nil)))
    (is (= "available" (.asString (.defaultInstallationPreference params))))
    (is (= #{:id :created-at :default-installation-preference :last-sync-ended-at
             :last-sync-read-sha :name :owner :source :sync-status :type}
           (set (keys result))))
    (is (= "Weather plugins" (:name result)))
    (is (= :available (:default-installation-preference result)))))

(deftest list-plugin-marketplaces-round-trips
  (let [[result params]
        (exercise-list-with!
         :plugin-marketplaces []
         "com.anthropic.services.blocking.beta.organization.PluginMarketplaceService"
         "com.anthropic.models.beta.organization.pluginmarketplaces.PluginMarketplaceListPage"
         "com.anthropic.models.beta.organization.pluginmarketplaces.PluginMarketplaceListPageResponse"
         [(marketplace) (marketplace)]
         #(organization/list-plugin-marketplaces
           % {:limit 20 :organization-id "org_1" :owner-type :user
               :page "page_2" :source :github}))]
    (is (= 20 (.orElse (.limit params) nil)))
    (is (= "org_1" (.orElse (.organizationId params) nil)))
    (is (= "user" (.asString (.orElse (.ownerType params) nil))))
    (is (= "page_2" (.orElse (.page params) nil)))
    (is (= "github" (.asString (.orElse (.source params) nil))))
    (is (= 2 (count result)))
    (is (= #{:id :created-at :default-installation-preference :last-sync-ended-at
             :last-sync-read-sha :name :owner :source :sync-status :type}
           (set (keys (first result)))))
    (is (= "pm_weather" (:id (first result))))
    (is (= :github (:source (first result))))))

(deftest validate-plugin-marketplace-archive-round-trips
  (let [payload (.getBytes "PK\\003\\004plugin archive" StandardCharsets/UTF_8)
        [result params]
        (exercise-single-with!
         :plugin-marketplaces
         "com.anthropic.services.blocking.beta.organization.PluginMarketplaceService"
         (validation-report)
         #(organization/validate-plugin-marketplace-archive % (ByteArrayInputStream. payload)))]
    (is (= "PK\\003\\004plugin archive"
           (String. (.readAllBytes (.archive params)) StandardCharsets/UTF_8)))
    (is (= #{:commit-sha :manifest-error :manifest-error-code :plugin-errors :plugin-warnings
             :ref :total-plugin-count :valid :type}
           (set (keys result))))
    (is (= "abc123" (:commit-sha result)))
    (is (= 2 (:total-plugin-count result)))
    (is (true? (:valid result)))))

(deftest validate-plugin-marketplace-repository-round-trips
  (let [[result params]
        (exercise-single-with!
         :plugin-marketplaces
         "com.anthropic.services.blocking.beta.organization.PluginMarketplaceService"
         (validation-report)
         #(organization/validate-plugin-marketplace-repository
           % "https://github.com/acme/weather-plugins" {:ref "main"}))]
    (is (= "https://github.com/acme/weather-plugins" (.repositoryUrl params)))
    (is (= "main" (.orElse (.ref params) nil)))
    (is (= #{:commit-sha :manifest-error :manifest-error-code :plugin-errors :plugin-warnings
             :ref :total-plugin-count :valid :type}
           (set (keys result))))
    (is (= "main" (:ref result)))
    (is (true? (:valid result)))))
