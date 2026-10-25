(ns anthropic.organization-claude-tag-test
  (:require [anthropic.organization :as organization]
            [anthropic.organization-test-support :as support]
            [clojure.test :refer [deftest is]])
  (:import (com.anthropic.models.beta.organization.claudetag BetaClaudeTagGuestPolicy
                                                            BetaClaudeTagGuestPolicyRestrict
                                                            BetaClaudeTagSettings
                                                            BetaClaudeTagSlackChannelIdentifiers
                                                            BetaClaudeTagSlackWorkspaceIdentifiers)
           (com.anthropic.models.beta.organization.claudetag.bundles.targets BetaClaudeTagBundleTarget
                                                                            BetaClaudeTagDeletedBundleTarget)
           (com.anthropic.models.beta.organization.claudetag.slackchannels BetaClaudeTagDeletedSlackChannel)
           (java.time OffsetDateTime)))

(def ^:private claude-tag "com.anthropic.services.blocking.beta.organization.ClaudeTagService")
(def ^:private bundles "com.anthropic.services.blocking.beta.organization.claudetag.BundleService")
(def ^:private targets "com.anthropic.services.blocking.beta.organization.claudetag.bundles.TargetService")
(def ^:private slack-channels "com.anthropic.services.blocking.beta.organization.claudetag.SlackChannelService")
(def ^:private slack-workspaces "com.anthropic.services.blocking.beta.organization.claudetag.SlackWorkspaceService")

(def ^:private timestamp (OffsetDateTime/parse "2026-10-01T09:00:00Z"))

(defn- present [optional] (.orElse optional nil))

(defn- settings []
  (-> ^BetaClaudeTagSettings (support/sdk-model "com.anthropic.models.beta.organization.claudetag.BetaClaudeTagSettings")
      (.toBuilder)
      (.enabled true)
      (.environmentId (java.util.Optional/empty))
      (.instructions (java.util.Optional/empty))
      (.model "claude-opus-5-5")
      (.guestPolicy (BetaClaudeTagGuestPolicy/ofRestrict
                     (.build (BetaClaudeTagGuestPolicyRestrict/builder))))
      (.build)))

(defn- slack-channel []
  (-> (support/sdk-model "com.anthropic.models.beta.organization.claudetag.slackchannels.BetaClaudeTagSlackChannel")
      (.toBuilder)
      (.id "ctsc_1")
      (.createdAt timestamp)
      (.updatedAt timestamp)
      (.settings (settings))
      (.slack (-> (BetaClaudeTagSlackChannelIdentifiers/builder)
                  (.channelId "C123") (.workspaceId "T456") (.build)))
      (.build)))

(defn- slack-workspace []
  (-> (support/sdk-model "com.anthropic.models.beta.organization.claudetag.slackworkspaces.BetaClaudeTagSlackWorkspace")
      (.toBuilder)
      (.id "ctsw_1")
      (.settings (settings))
      (.slack (BetaClaudeTagSlackWorkspaceIdentifiers/of "T456"))
      (.build)))

(defn- bundle []
  (-> (support/sdk-model "com.anthropic.models.beta.organization.claudetag.bundles.BetaClaudeTagBundle")
      (.toBuilder)
      (.id "ctb_1")
      (.displayName "Support bundle")
      (.description "Bundled settings")
      (.createdAt timestamp)
      (.build)))

(defn- bundle-target []
  (BetaClaudeTagBundleTarget/ofSlackChannel
   (-> (support/sdk-model "com.anthropic.models.beta.organization.claudetag.bundles.targets.BetaClaudeTagSlackChannelBundleTarget")
       (.toBuilder)
       (.id "ctbt_1")
       (.slackChannelId "C123")
       (.createdAt timestamp)
       (.build))))

;; ---- Slack channels -------------------------------------------------------

(deftest claude-tag-slack-channel-create-builds-params
  (let [[result params]
        (support/exercise-path! :claude-tag [claude-tag] slack-channels (slack-channel)
                                #(organization/create-claude-tag-slack-channel
                                  % {:slack {:channel-id "C123" :workspace-id "T456"}
                                     :settings {:enabled true
                                                :model "claude-opus-5-5"
                                                :guest-policy {:type :restrict}}}))]
    (is (= "C123" (.channelId (.slack params))))
    (is (= "T456" (.workspaceId (.slack params))))
    (is (= true (present (.enabled (present (.settings params))))))
    (is (= "claude-opus-5-5" (present (.model (present (.settings params))))))
    (is (.isRestrict (present (.guestPolicy (present (.settings params))))))
    (is (= "ctsc_1" (:id result)))))

(deftest claude-tag-slack-channel-create-requires-slack
  (let [error (try (organization/create-claude-tag-slack-channel
                    (support/beta-client {}) {:settings {:enabled true}})
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
    (is (= :missing-key (:anthropic/error (ex-data error))))
    (is (= :slack (:key (ex-data error))))))

(deftest claude-tag-slack-channel-round-trips
  (let [[result] (support/exercise-path! :claude-tag [claude-tag] slack-channels (slack-channel)
                                         #(organization/get-claude-tag-slack-channel % "ctsc_1"))]
    (is (= #{:id :created-at :settings :slack :sources :type :updated-at} (set (keys result))))
    (is (= "ctsc_1" (:id result)))
    (is (= "2026-10-01T09:00:00Z" (:created-at result)))
    (is (= {:channel-id "C123" :workspace-id "T456"} (:slack result)))
    (is (= {:enabled true :model "claude-opus-5-5" :guest-policy {:type :restrict}
            :environment-id nil :instructions nil}
           (:settings result)))))

(deftest claude-tag-slack-channel-get-builds-params
  (let [[_ params] (support/exercise-path! :claude-tag [claude-tag] slack-channels (slack-channel)
                                           #(organization/get-claude-tag-slack-channel
                                             % "ctsc_1" {:include-inherited true}))]
    (is (= "ctsc_1" (present (.id params))))
    (is (= true (present (.includeInherited params))))))

(deftest claude-tag-slack-channel-update-builds-params
  (let [[result params]
        (support/exercise-path! :claude-tag [claude-tag] slack-channels (slack-channel)
                                #(organization/update-claude-tag-slack-channel
                                  % "ctsc_1" {:settings {:instructions "Be brief"
                                                         :guest-policy {:type :channel-only}}}))]
    (is (= "ctsc_1" (present (.id params))))
    (is (= "Be brief" (present (.instructions (.settings params)))))
    (is (.isChannelOnly (present (.guestPolicy (.settings params)))))
    (is (= "ctsc_1" (:id result)))))

(deftest claude-tag-slack-channel-list-builds-params
  (let [[result params]
        (support/exercise-path-list!
         :claude-tag [claude-tag] slack-channels
         "com.anthropic.models.beta.organization.claudetag.slackchannels.SlackChannelListPage"
         "com.anthropic.models.beta.organization.claudetag.slackchannels.SlackChannelListPageResponse"
         [(slack-channel) (slack-channel)]
         #(organization/list-claude-tag-slack-channels
           % {:slack-workspace-id "T456" :slack-channel-id "C123"
              :include-inherited true :limit 10 :page "p2"}))]
    (is (= "T456" (.slackWorkspaceId params)))
    (is (= "C123" (present (.slackChannelId params))))
    (is (= true (present (.includeInherited params))))
    (is (= 10 (present (.limit params))))
    (is (= "p2" (present (.page params))))
    (is (= ["ctsc_1" "ctsc_1"] (mapv :id result)))))

(deftest claude-tag-slack-channel-list-requires-slack-workspace-id
  (let [error (try (organization/list-claude-tag-slack-channels (support/beta-client {}) {})
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
    (is (= :missing-key (:anthropic/error (ex-data error))))
    (is (= :slack-workspace-id (:key (ex-data error))))))

(deftest claude-tag-slack-channel-delete-round-trips
  (let [[result params]
        (support/exercise-path! :claude-tag [claude-tag] slack-channels
                                (BetaClaudeTagDeletedSlackChannel/of "ctsc_1")
                                #(organization/delete-claude-tag-slack-channel % "ctsc_1"))]
    (is (= "ctsc_1" (present (.id params))))
    (is (= "ctsc_1" (:id result)))))

;; ---- Slack workspaces -----------------------------------------------------

(deftest claude-tag-slack-workspace-round-trips
  (let [[result params]
        (support/exercise-path! :claude-tag [claude-tag] slack-workspaces (slack-workspace)
                                #(organization/get-claude-tag-slack-workspace
                                  % "ctsw_1" {:include-inherited false}))]
    (is (= "ctsw_1" (present (.id params))))
    (is (= false (present (.includeInherited params))))
    (is (= #{:id :created-at :settings :slack :sources :type :updated-at} (set (keys result))))
    (is (= {:workspace-id "T456"} (:slack result)))
    (is (= :restrict (get-in result [:settings :guest-policy :type])))))

(deftest claude-tag-slack-workspace-update-builds-params
  (let [[result params]
        (support/exercise-path! :claude-tag [claude-tag] slack-workspaces (slack-workspace)
                                #(organization/update-claude-tag-slack-workspace
                                  % "ctsw_1" {:settings {:enabled false :environment-id "env_1"}}))]
    (is (= "ctsw_1" (present (.id params))))
    (is (= false (present (.enabled (.settings params)))))
    (is (= "env_1" (present (.environmentId (.settings params)))))
    (is (= "ctsw_1" (:id result)))))

(deftest claude-tag-slack-workspace-list-builds-params
  (let [[result params]
        (support/exercise-path-list!
         :claude-tag [claude-tag] slack-workspaces
         "com.anthropic.models.beta.organization.claudetag.slackworkspaces.SlackWorkspaceListPage"
         "com.anthropic.models.beta.organization.claudetag.slackworkspaces.SlackWorkspaceListPageResponse"
         [(slack-workspace)]
         #(organization/list-claude-tag-slack-workspaces
           % {:slack-workspace-id "T456" :include-inherited true :limit 5}))]
    (is (= "T456" (present (.slackWorkspaceId params))))
    (is (= true (present (.includeInherited params))))
    (is (= 5 (present (.limit params))))
    (is (= ["ctsw_1"] (mapv :id result)))))

;; ---- Bundles and targets --------------------------------------------------

(deftest claude-tag-bundle-round-trips
  (let [[result params]
        (support/exercise-path! :claude-tag [claude-tag] bundles (bundle)
                                #(organization/get-claude-tag-bundle % "ctb_1"))]
    (is (= "ctb_1" (present (.bundleId params))))
    (is (= #{:id :created-at :description :display-name :type :updated-at} (set (keys result))))
    (is (= "Support bundle" (:display-name result)))
    (is (= "Bundled settings" (:description result)))))

(deftest claude-tag-bundle-list-builds-params
  (let [[result params]
        (support/exercise-path-list!
         :claude-tag [claude-tag] bundles
         "com.anthropic.models.beta.organization.claudetag.bundles.BundleListPage"
         "com.anthropic.models.beta.organization.claudetag.bundles.BundleListPageResponse"
         [(bundle) (bundle)]
         #(organization/list-claude-tag-bundles % {:limit 20 :page "b2"}))]
    (is (= 20 (present (.limit params))))
    (is (= "b2" (present (.page params))))
    (is (= ["ctb_1" "ctb_1"] (mapv :id result)))))

(deftest claude-tag-bundle-target-create-builds-params
  (let [[result params]
        (support/exercise-path! :claude-tag [claude-tag bundles] targets (bundle-target)
                                #(organization/create-claude-tag-bundle-target
                                  % "ctb_1" {:target {:type :slack-channel :slack-channel-id "C123"}}))]
    (is (= "ctb_1" (present (.bundleId params))))
    (is (.isSlackChannel (.target params)))
    (is (= "C123" (.slackChannelId (.asSlackChannel (.target params)))))
    (is (= {:id "ctbt_1" :type :slack-channel :slack-channel-id "C123"
            :created-at "2026-10-01T09:00:00Z"}
           result))))

(deftest claude-tag-bundle-target-create-requires-target
  (let [error (try (organization/create-claude-tag-bundle-target
                    (support/beta-client {}) "ctb_1" {})
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
    (is (= :missing-key (:anthropic/error (ex-data error))))
    (is (= :target (:key (ex-data error))))))

(deftest claude-tag-bundle-target-get-builds-params
  (let [[result params]
        (support/exercise-path! :claude-tag [claude-tag bundles] targets (bundle-target)
                                #(organization/get-claude-tag-bundle-target % "ctb_1" "ctbt_1"))]
    (is (= "ctb_1" (.bundleId params)))
    (is (= "ctbt_1" (present (.targetId params))))
    (is (= :slack-channel (:type result)))))

(deftest claude-tag-bundle-target-list-builds-params
  (let [[result params]
        (support/exercise-path-list!
         :claude-tag [claude-tag bundles] targets
         "com.anthropic.models.beta.organization.claudetag.bundles.targets.TargetListPage"
         "com.anthropic.models.beta.organization.claudetag.bundles.targets.TargetListPageResponse"
         [(bundle-target)]
         #(organization/list-claude-tag-bundle-targets % "ctb_1" {:limit 3}))]
    (is (= "ctb_1" (present (.bundleId params))))
    (is (= 3 (present (.limit params))))
    (is (= ["ctbt_1"] (mapv :id result)))))

(deftest claude-tag-bundle-target-delete-round-trips
  (let [[result params]
        (support/exercise-path! :claude-tag [claude-tag bundles] targets
                                (BetaClaudeTagDeletedBundleTarget/of "ctbt_1")
                                #(organization/delete-claude-tag-bundle-target % "ctb_1" "ctbt_1"))]
    (is (= "ctb_1" (.bundleId params)))
    (is (= "ctbt_1" (present (.targetId params))))
    (is (= "ctbt_1" (:id result)))))
