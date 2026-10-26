(ns anthropic.organization-telemetry-webhooks-test
  (:require [anthropic.organization :as organization]
            [anthropic.organization-test-support :as support]
            [clojure.test :refer [deftest is]])
  (:import (com.anthropic.models.beta.organization.telemetrydestinations BetaDeletedTelemetryDestination
                                                                        BetaTelemetryDestinationTestResponse
                                                                        BetaTelemetryDestinationTestResult
                                                                        BetaTelemetryDestinationTestSuccess)
           (com.anthropic.models.beta.organization.webhooks BetaDeletedOrganizationWebhookEndpoint
                                                           BetaOrganizationWebhookEndpointSigningSecret
                                                           WebhookCreateParams$EnabledEvent
                                                           WebhookListParams$Order
                                                           WebhookUpdateParams$State)
           (java.time OffsetDateTime)))

(def ^:private destinations "com.anthropic.services.blocking.beta.organization.TelemetryDestinationService")
(def ^:private destination-workspaces "com.anthropic.services.blocking.beta.organization.telemetrydestinations.WorkspaceService")
(def ^:private webhooks "com.anthropic.services.blocking.beta.organization.WebhookService")

(def ^:private timestamp (OffsetDateTime/parse "2026-10-02T08:00:00Z"))

(defn- present [optional] (.orElse optional nil))

(defn- missing-key [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (select-keys (ex-data e) [:anthropic/error :key]))))

(defn- destination []
  (support/from-json
   "com.anthropic.models.beta.organization.telemetrydestinations.BetaTelemetryDestination"
   {"id" "td_1"
    "type" "telemetry_destination"
    "created_at" "2026-10-02T08:00:00Z"
    "updated_at" "2026-10-02T08:00:00Z"
    "display_name" "Collector"
    "endpoint_url" "https://otel.example.com/v1"
    "header_names" ["Authorization"]
    "delivery_health" {"last_error" nil "last_error_at" nil
                       "last_success_at" "2026-10-02T08:05:00Z"}
    "state" {"type" "enabled"}
    "traces" {"enabled" true "content_mode" "redacted"
              "endpoint_url" nil "max_content_bytes" 2048}
    "workspaces" {"type" "include" "workspace_ids" ["wrkspc_1"]}}))

(defn- destination-workspace []
  (-> (support/sdk-model "com.anthropic.models.beta.organization.telemetrydestinations.workspaces.BetaTelemetryDestinationWorkspace")
      (.toBuilder)
      (.telemetryDestinationId "td_1")
      (.workspaceId "wrkspc_1")
      (.createdAt timestamp)
      (.build)))

(defn- endpoint [class-name]
  (-> (support/sdk-model class-name)
      (.toBuilder)
      (.id "whep_1")
      (.name "Enforcement")
      (.url "https://hooks.example.com/anthropic")
      (.createdAt timestamp)
      (.build)))

;; ---- Telemetry destinations -----------------------------------------------

(deftest create-telemetry-destination-builds-params
  (let [[result params]
        (support/exercise-path! :telemetry-destinations [] destinations (destination)
                                #(organization/create-telemetry-destination
                                  % {:endpoint-url "https://otel.example.com/v1"
                                     :display-name "Collector"
                                     :workspaces {:type :include :workspace-ids ["wrkspc_1"]}
                                     :headers {"Authorization" "Bearer s3cret"}
                                     :traces {:enabled true :max-content-bytes 2048}}))]
    (is (= "https://otel.example.com/v1" (.endpointUrl params)))
    (is (= "Collector" (present (.displayName params))))
    (is (.isInclude (.workspaces params)))
    (is (= ["wrkspc_1"] (vec (present (.workspaceIds (.asInclude (.workspaces params)))))))
    (is (= "Bearer s3cret"
           (str (get (._additionalProperties (present (.headers params))) "Authorization"))))
    (is (= true (present (.enabled (present (.traces params))))))
    (is (= 2048 (present (.maxContentBytes (present (.traces params))))))
    (is (= "td_1" (:id result)))))

(deftest create-telemetry-destination-requires-endpoint-url-and-workspaces
  (is (= {:anthropic/error :missing-key :key :endpoint-url}
         (missing-key #(organization/create-telemetry-destination
                        (support/beta-client {}) {:workspaces {:type :all}}))))
  (is (= {:anthropic/error :missing-key :key :workspaces}
         (missing-key #(organization/create-telemetry-destination
                        (support/beta-client {}) {:endpoint-url "https://x"})))))

(deftest telemetry-destination-round-trips
  (let [[result params]
        (support/exercise-path! :telemetry-destinations [] destinations (destination)
                                #(organization/get-telemetry-destination % "td_1"))]
    (is (= "td_1" (present (.destinationId params))))
    (is (= #{:id :created-at :delivery-health :display-name :endpoint-url :header-names
             :state :traces :type :updated-at :workspaces}
           (set (keys result))))
    (is (= "https://otel.example.com/v1" (:endpoint-url result)))
    (is (= ["Authorization"] (:header-names result)))
    (is (= "2026-10-02T08:00:00Z" (:created-at result)))
    (is (= {:type :enabled} (:state result)))
    (is (= {:type :include :workspace-ids ["wrkspc_1"]} (:workspaces result)))
    (is (= {:enabled true :content-mode :redacted :max-content-bytes 2048}
           (:traces result)))
    (is (= "2026-10-02T08:05:00Z" (get-in result [:delivery-health :last-success-at])))))

(deftest list-telemetry-destinations-builds-params
  (let [[result params]
        (support/exercise-path-list!
         :telemetry-destinations [] destinations
         "com.anthropic.models.beta.organization.telemetrydestinations.TelemetryDestinationListPage"
         "com.anthropic.models.beta.organization.telemetrydestinations.TelemetryDestinationListPageResponse"
         [(destination) (destination)]
         #(organization/list-telemetry-destinations
           % {:limit 10 :page "p2" :workspace-ids ["wrkspc_1" "wrkspc_2"]}))]
    (is (= 10 (present (.limit params))))
    (is (= "p2" (present (.page params))))
    (is (= ["wrkspc_1" "wrkspc_2"] (vec (present (.workspaceIds params)))))
    (is (= ["td_1" "td_1"] (mapv :id result)))))

(deftest update-telemetry-destination-builds-params
  (let [[result params]
        (support/exercise-path! :telemetry-destinations [] destinations (destination)
                                #(organization/update-telemetry-destination
                                  % "td_1" {:state {:type :disabled}
                                            :workspaces {:type :all}
                                            :display-name "Paused"}))]
    (is (= "td_1" (present (.destinationId params))))
    (is (.isDisabled (present (.state params))))
    (is (.isAll (present (.workspaces params))))
    (is (= "Paused" (present (.displayName params))))
    (is (= "td_1" (:id result)))))

(deftest delete-telemetry-destination-round-trips
  (let [[result params]
        (support/exercise-path! :telemetry-destinations [] destinations
                                (BetaDeletedTelemetryDestination/of "td_1")
                                #(organization/delete-telemetry-destination % "td_1"))]
    (is (= "td_1" (present (.destinationId params))))
    (is (= "td_1" (:id result)))))

(deftest test-telemetry-destination-round-trips
  (let [response (BetaTelemetryDestinationTestResponse/of
                  (BetaTelemetryDestinationTestResult/ofSuccess
                   (.build (BetaTelemetryDestinationTestSuccess/builder))))
        [result params]
        (support/exercise-path! :telemetry-destinations [] destinations response
                                #(organization/test-telemetry-destination % "td_1"))]
    (is (= "td_1" (present (.destinationId params))))
    (is (= :success (get-in result [:result :type])))))

(deftest add-telemetry-destination-workspace-builds-params
  (let [[result params]
        (support/exercise-path! :telemetry-destinations [destinations] destination-workspaces
                                (destination-workspace)
                                #(organization/add-telemetry-destination-workspace % "td_1" "wrkspc_1"))]
    (is (= "td_1" (.destinationId params)))
    (is (= "wrkspc_1" (present (.workspaceId params))))
    (is (= #{:created-at :telemetry-destination-id :type :workspace-id} (set (keys result))))
    (is (= {:telemetry-destination-id "td_1" :workspace-id "wrkspc_1"}
           (select-keys result [:telemetry-destination-id :workspace-id])))))

(deftest list-telemetry-destination-workspaces-builds-params
  (let [[result params]
        (support/exercise-path-list!
         :telemetry-destinations [destinations] destination-workspaces
         "com.anthropic.models.beta.organization.telemetrydestinations.workspaces.WorkspaceListPage"
         "com.anthropic.models.beta.organization.telemetrydestinations.workspaces.WorkspaceListPageResponse"
         [(destination-workspace)]
         #(organization/list-telemetry-destination-workspaces % "td_1" {:limit 4}))]
    (is (= "td_1" (present (.destinationId params))))
    (is (= 4 (present (.limit params))))
    (is (= ["wrkspc_1"] (mapv :workspace-id result)))))

(deftest remove-telemetry-destination-workspace-round-trips
  (let [response (-> (support/sdk-model "com.anthropic.models.beta.organization.telemetrydestinations.workspaces.BetaDeletedTelemetryDestinationWorkspace")
                     (.toBuilder) (.workspaceId "wrkspc_1") (.build))
        [result params]
        (support/exercise-path! :telemetry-destinations [destinations] destination-workspaces response
                                #(organization/remove-telemetry-destination-workspace % "td_1" "wrkspc_1"))]
    (is (= "td_1" (.destinationId params)))
    (is (= "wrkspc_1" (present (.workspaceId params))))
    (is (= "wrkspc_1" (:workspace-id result)))))

;; ---- Organization webhook endpoints ---------------------------------------

(deftest create-webhook-endpoint-builds-params
  (let [[result params]
        (support/exercise-path! :webhooks [] webhooks
                                (-> (endpoint "com.anthropic.models.beta.organization.webhooks.BetaNewOrganizationWebhookEndpoint")
                                    (.toBuilder) (.signingSecret "whsec_1") (.build))
                                #(organization/create-webhook-endpoint
                                  % {:url "https://hooks.example.com/anthropic"
                                     :name "Enforcement"
                                     :description "Profile enforcement"
                                     :enabled-events [:user-profile-enforcement-action-opened
                                                      :user-profile-enforcement-action-resolved]}))]
    (is (= "https://hooks.example.com/anthropic" (.url params)))
    (is (= "Enforcement" (present (.name params))))
    (is (= "Profile enforcement" (present (.description params))))
    (is (= [WebhookCreateParams$EnabledEvent/USER_PROFILE_ENFORCEMENT_ACTION_OPENED
            WebhookCreateParams$EnabledEvent/USER_PROFILE_ENFORCEMENT_ACTION_RESOLVED]
           (vec (.enabledEvents params))))
    (is (= "whep_1" (:id result)))
    (is (= "whsec_1" (:signing-secret result)))))

(deftest create-webhook-endpoint-requires-url-and-enabled-events
  (is (= {:anthropic/error :missing-key :key :url}
         (missing-key #(organization/create-webhook-endpoint
                        (support/beta-client {}) {:enabled-events [:user-profile-enforcement-action-opened]}))))
  (is (= {:anthropic/error :missing-key :key :enabled-events}
         (missing-key #(organization/create-webhook-endpoint
                        (support/beta-client {}) {:url "https://x"})))))

(deftest webhook-endpoint-round-trips
  (let [[result params]
        (support/exercise-path! :webhooks [] webhooks
                                (endpoint "com.anthropic.models.beta.organization.webhooks.BetaOrganizationWebhookEndpoint")
                                #(organization/get-webhook-endpoint % "whep_1"))]
    (is (= "whep_1" (present (.webhookEndpointId params))))
    (is (= #{:id :created-at :description :disabled-reason :enabled-events :name :state
             :updated-at :url}
           (set (keys result))))
    (is (= "https://hooks.example.com/anthropic" (:url result)))
    (is (= "Enforcement" (:name result)))
    (is (keyword? (:state result)))))

(deftest list-webhook-endpoints-builds-params
  (let [item (endpoint "com.anthropic.models.beta.organization.webhooks.BetaOrganizationWebhookEndpoint")
        [result params]
        (support/exercise-path-list!
         :webhooks [] webhooks
         "com.anthropic.models.beta.organization.webhooks.WebhookListPage"
         "com.anthropic.models.beta.organization.webhooks.WebhookListPageResponse"
         [item item]
         #(organization/list-webhook-endpoints
           % {:limit 2 :after-id "whep_0" :order :desc}))]
    (is (= 2 (present (.limit params))))
    (is (= "whep_0" (present (.afterId params))))
    (is (= WebhookListParams$Order/DESC (present (.order params))))
    (is (= ["whep_1" "whep_1"] (mapv :id result)))))

(deftest update-webhook-endpoint-builds-params
  (let [[result params]
        (support/exercise-path! :webhooks [] webhooks
                                (endpoint "com.anthropic.models.beta.organization.webhooks.BetaOrganizationWebhookEndpoint")
                                #(organization/update-webhook-endpoint
                                  % "whep_1" {:state :disabled :name "Paused"}))]
    (is (= "whep_1" (present (.webhookEndpointId params))))
    (is (= WebhookUpdateParams$State/DISABLED (present (.state params))))
    (is (= "Paused" (present (.name params))))
    (is (= "whep_1" (:id result)))))

(deftest delete-webhook-endpoint-round-trips
  (let [[result params]
        (support/exercise-path! :webhooks [] webhooks
                                (BetaDeletedOrganizationWebhookEndpoint/of "whep_1")
                                #(organization/delete-webhook-endpoint % "whep_1"))]
    (is (= "whep_1" (present (.webhookEndpointId params))))
    (is (= "whep_1" (:id result)))))

(deftest regenerate-webhook-signing-secret-round-trips
  (let [[result params]
        (support/exercise-path! :webhooks [] webhooks
                                (BetaOrganizationWebhookEndpointSigningSecret/of "whsec_2")
                                #(organization/regenerate-webhook-signing-secret % "whep_1"))]
    (is (= "whep_1" (present (.webhookEndpointId params))))
    (is (= "whsec_2" (:signing-secret result)))))
