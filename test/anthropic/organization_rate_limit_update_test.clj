(ns anthropic.organization-rate-limit-update-test
  (:require [anthropic.organization :as organization]
            [anthropic.organization-test-support :as support]
            [clojure.test :refer [deftest is]])
  (:import (com.anthropic.models.beta.organization.workspaces.ratelimits BetaWorkspaceRateLimitType)
           (com.anthropic.models.organization.workspaces.ratelimits WorkspaceRateLimitType)))

(def ^:private rate-limit-json
  {"type" "workspace_rate_limit"
   "rate_limit_id" "rl_1"
   "workspace_id" "wrkspc_1"
   "group" {"type" "model_group" "id" "mg_1" "display_name" "Claude models"}
   "models" ["claude-opus-5-5"]
   "limits" [{"type" "requests_per_minute" "value" 100 "org_limit" 500
              "source" {"type" "workspace"}}]})

(defn- present [optional] (.orElse optional nil))

(defn- stable-update! [response invoke]
  (let [captured (atom nil)
        leaf (support/leaf-service
              "com.anthropic.services.blocking.organization.workspaces.RateLimitService"
              response captured)
        organization (support/path-service
                      ["com.anthropic.services.blocking.OrganizationService"
                       "com.anthropic.services.blocking.organization.WorkspaceService"]
                      leaf)]
    [(invoke (support/stable-client {:organization organization})) @captured]))

(deftest update-workspace-rate-limit-builds-params
  (let [[result params]
        (stable-update! (support/from-json
                         "com.anthropic.models.organization.workspaces.ratelimits.WorkspaceRateLimit"
                         rate-limit-json)
                        #(organization/update-workspace-rate-limit
                          % "wrkspc_1" "rl_1"
                          {:limits [{:type :requests-per-minute :value 100}
                                    {:type :output-tokens-per-minute :value nil}]}))]
    (is (= "wrkspc_1" (.workspaceId params)))
    (is (= "rl_1" (present (.rateLimitId params))))
    (is (= [WorkspaceRateLimitType/REQUESTS_PER_MINUTE WorkspaceRateLimitType/OUTPUT_TOKENS_PER_MINUTE]
           (mapv #(.type %) (.limits params))))
    (is (= [100 nil] (mapv #(present (.value %)) (.limits params))))
    (is (= {:rate-limit-id "rl_1"
            :workspace-id "wrkspc_1"
            :group {:id "mg_1" :type "model-group" :display-name "Claude models"}
            :limits [{:type "requests_per_minute" :value 100 :org-limit 500
                      :source {:type :workspace}}]
            :models ["claude-opus-5-5"]}
           result))))

(deftest update-workspace-rate-limit-requires-limits
  (let [error (try (organization/update-workspace-rate-limit
                    (support/stable-client {}) "wrkspc_1" "rl_1" {})
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
    (is (= {:anthropic/error :missing-key :key :limits}
           (select-keys (ex-data error) [:anthropic/error :key])))))

(deftest update-beta-workspace-rate-limit-builds-params
  (let [[result params]
        (support/exercise-path!
         :workspaces ["com.anthropic.services.blocking.beta.organization.WorkspaceService"]
         "com.anthropic.services.blocking.beta.organization.workspaces.RateLimitService"
         (support/from-json
          "com.anthropic.models.beta.organization.workspaces.ratelimits.BetaWorkspaceRateLimit"
          rate-limit-json)
         #(organization/update-beta-workspace-rate-limit
           % "wrkspc_1" "rl_1" {:limits [{:type :tokens-per-day :value 10000}]}))]
    (is (= "wrkspc_1" (.workspaceId params)))
    (is (= "rl_1" (present (.rateLimitId params))))
    (is (= [BetaWorkspaceRateLimitType/TOKENS_PER_DAY] (mapv #(.type %) (.limits params))))
    (is (= "rl_1" (:rate-limit-id result)))
    (is (= {:type :model-group :id "mg_1" :display-name "Claude models"} (:group result)))
    (is (= [{:type "requests_per_minute" :value 100 :org-limit 500 :source {:type :workspace}}]
           (:limits result)))))

(deftest update-beta-workspace-rate-limit-requires-limits
  (let [error (try (organization/update-beta-workspace-rate-limit
                    (support/beta-client {}) "wrkspc_1" "rl_1" {})
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
    (is (= {:anthropic/error :missing-key :key :limits}
           (select-keys (ex-data error) [:anthropic/error :key])))))
