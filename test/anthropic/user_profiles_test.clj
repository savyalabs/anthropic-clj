(ns anthropic.user-profiles-test
  (:require [anthropic.beta :as beta]
            [anthropic.core :as a]
            [anthropic.organization-test-support :as support]
            [clojure.test :refer [deftest is]])
  (:import (com.anthropic.client AnthropicClient)
           (com.anthropic.models.userprofiles UserProfileAccessType
                                              UserProfileEnforcementStatus
                                              UserProfileExternalUserAccountStatus
                                              UserProfileListOrder
                                              UserProfileListOrderBy)
           (com.anthropic.services.blocking BetaService)))

(def ^:private profile-json
  {"id" "uprof_1"
   "type" "user_profile"
   "name" "Ada"
   "access_type" "application"
   "created_at" "2026-10-03T10:00:00Z"
   "updated_at" "2026-10-04T10:00:00Z"
   "metadata" {"team" "research"}
   "external_user_details" {"account_status" "active" "country" "GB"
                            "entity_type" "individual"}
   "enforcement_status" "restricted"
   "enforcement_actions" [{"type" "restriction"
                           "id" "enf_1"
                           "opened_at" "2026-10-03T11:00:00Z"
                           "expires_at" nil
                           "reason" "unsupported_region"
                           "action" "verify"}]
   "programs" {"cyber" {"status" "active"
                        "requirements" {"data_retention" true "human_review" false}
                        "tiers" {"tier_1" {"status" "approved"}}}
               "efs" {"status" "pending"
                      "features" {"sandbox" {"status" "enabled"}}}}})

(defn- profile []
  (support/from-json "com.anthropic.models.userprofiles.UserProfile" profile-json))

(defn- present [optional] (.orElse optional nil))

(def ^:private expected-profile
  {:id "uprof_1"
   :type :user-profile
   :name "Ada"
   :access-type :application
   :created-at "2026-10-03T10:00:00Z"
   :updated-at "2026-10-04T10:00:00Z"
   :metadata {:team "research"}
   :external-user-details {:account-status :active :country "GB" :entity-type :individual}
   :enforcement-status :restricted
   :enforcement-actions [{:type :restriction
                          :id "enf_1"
                          :opened-at "2026-10-03T11:00:00Z"
                          :reason :unsupported-region
                          :action :verify}]
   :programs {:cyber {:status :active
                      :requirements {:data-retention true :human-review false}
                      :tiers {:tier-1 {:status :approved}}}
              :efs {:status :pending
                    :features {:sandbox {:status :enabled}}}}})

(defn- stable-service [captured responses]
  (support/proxy-service
   (Class/forName "com.anthropic.services.blocking.UserProfileService")
   (fn [method [params]]
     (swap! captured assoc method params)
     ((get responses method) params))))

(deftest user-profiles-stable-crud-round-trips
  (let [captured (atom {})
        page-service (support/proxy-service
                      (Class/forName "com.anthropic.services.blocking.UserProfileService")
                      (fn [_ _] nil))
        service (stable-service
                 captured
                 {"create" (fn [_] (profile))
                  "retrieve" (fn [_] (profile))
                  "update" (fn [_] (profile))
                  "list" (fn [params]
                           (support/list-page
                            "com.anthropic.models.userprofiles.UserProfileListPage"
                            "com.anthropic.models.userprofiles.UserProfileListPageResponse"
                            page-service params [(profile)]))
                  "delete" (fn [_] (support/from-json
                                    "com.anthropic.models.userprofiles.DeletedUserProfile"
                                    {"id" "uprof_1" "type" "user_profile_deleted"}))})
        client (support/stable-client {:user-profiles service})
        created (a/create-user-profile client {:name "Ada"
                                               :access-type :application
                                               :metadata {:team "research"}
                                               :external-user-details {:account-status :active
                                                                       :country "GB"}
                                               :workspace-id "wrkspc_1"})
        fetched (a/get-user-profile client "uprof_1" {:workspace-id "wrkspc_1"})
        listed (a/list-user-profiles client {:enforcement-status [:restricted :banned]
                                             :order :desc :order-by :created-at
                                             :limit 5 :page "p2" :workspace-id "wrkspc_1"})
        updated (a/update-user-profile client "uprof_1" {:name "Ada L" :access-type :passthrough})
        deleted (a/delete-user-profile client "uprof_1" {:workspace-id "wrkspc_1"})
        create-params (get @captured "create")
        retrieve-params (get @captured "retrieve")
        list-params (get @captured "list")
        update-params (get @captured "update")
        delete-params (get @captured "delete")]
    (is (= expected-profile created fetched updated))
    (is (= [expected-profile] listed))
    (is (= {:id "uprof_1" :type :user-profile-deleted} deleted))
    (is (= "Ada" (present (.name create-params))))
    (is (= UserProfileAccessType/APPLICATION (present (.accessType create-params))))
    (is (= "research" (str (get (._additionalProperties (present (.metadata create-params))) "team"))))
    (is (= UserProfileExternalUserAccountStatus/ACTIVE
           (present (.accountStatus (present (.externalUserDetails create-params))))))
    (is (= "wrkspc_1" (present (.workspaceId create-params))))
    (is (empty? (.values (._headers create-params) "anthropic-beta"))
        "stable params carry no beta header")
    (is (= "uprof_1" (present (.userProfileId retrieve-params))))
    (is (= "wrkspc_1" (present (.workspaceId retrieve-params))))
    (is (= [UserProfileEnforcementStatus/RESTRICTED UserProfileEnforcementStatus/BANNED]
           (present (.enforcementStatus list-params))))
    (is (= UserProfileListOrder/DESC (present (.order list-params))))
    (is (= UserProfileListOrderBy/CREATED_AT (present (.orderBy list-params))))
    (is (= 5 (present (.limit list-params))))
    (is (= "p2" (present (.page list-params))))
    (is (= "uprof_1" (present (.userProfileId update-params))))
    (is (= "Ada L" (present (.name update-params))))
    (is (= UserProfileAccessType/PASSTHROUGH (present (.accessType update-params))))
    (is (= "uprof_1" (present (.userProfileId delete-params))))
    (is (= "wrkspc_1" (present (.workspaceId delete-params))))))

(deftest create-user-profile-action-url-builds-params
  (let [captured (atom nil)
        leaf (support/leaf-service
              "com.anthropic.services.blocking.userprofiles.ActionUrlService"
              (support/from-json "com.anthropic.models.userprofiles.actionurls.UserProfileActionUrl"
                                 {"type" "user_profile_action_url"
                                  "url" "https://claude.ai/enroll/abc"
                                  "expires_at" "2026-10-05T00:00:00Z"})
              captured)
        service (support/path-service ["com.anthropic.services.blocking.UserProfileService"] leaf)
        result (a/create-user-profile-action-url (support/stable-client {:user-profiles service})
                                                 "uprof_1" {:workspace-id "wrkspc_1"})
        params @captured]
    (is (= "uprof_1" (present (.userProfileId params))))
    (is (= "wrkspc_1" (present (.workspaceId params))))
    (is (= "enroll_end_user" (str (._type (.action params)))))
    (is (= {:url "https://claude.ai/enroll/abc" :expires-at "2026-10-05T00:00Z"
            :type :user-profile-action-url}
           result))))

(deftest create-user-profile-action-url-rejects-unknown-action
  (let [error (try (a/create-user-profile-action-url (support/stable-client {}) "uprof_1"
                                                     {:action :delete-everything})
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
    (is (= :invalid-enum-value (:anthropic/error (ex-data error))))))

;; ---- beta user profiles -----------------------------------------------------

(defn- beta-user-profiles-client [service]
  (reify AnthropicClient
    (beta [_] (reify BetaService (userProfiles [_] service)))))

(deftest beta-delete-user-profile-builds-params
  (let [captured (atom nil)
        service (support/leaf-service
                 "com.anthropic.services.blocking.beta.UserProfileService"
                 (support/from-json "com.anthropic.models.beta.userprofiles.BetaDeletedUserProfile"
                                    {"id" "uprof_1" "type" "user_profile_deleted"})
                 captured)
        result (beta/delete-user-profile (beta-user-profiles-client service) "uprof_1"
                                         {:workspace-id "wrkspc_1"})]
    (is (= "uprof_1" (present (.userProfileId @captured))))
    (is (= "wrkspc_1" (present (.workspaceId @captured))))
    (is (= {:id "uprof_1" :type :user-profile-deleted} result))))

(deftest beta-create-user-profile-action-url-builds-params
  (let [captured (atom nil)
        leaf (support/leaf-service
              "com.anthropic.services.blocking.beta.userprofiles.ActionUrlService"
              (support/from-json "com.anthropic.models.beta.userprofiles.actionurls.BetaUserProfileActionUrl"
                                 {"type" "user_profile_action_url"
                                  "url" "https://claude.ai/enroll/xyz"
                                  "expires_at" "2026-10-05T00:00:00Z"})
              captured)
        service (support/path-service ["com.anthropic.services.blocking.beta.UserProfileService"] leaf)
        result (beta/create-user-profile-action-url (beta-user-profiles-client service) "uprof_1")]
    (is (= "uprof_1" (present (.userProfileId @captured))))
    (is (= "enroll_end_user" (str (._type (.action @captured)))))
    (is (= {:url "https://claude.ai/enroll/xyz" :expires-at "2026-10-05T00:00Z"
            :type :user-profile-action-url}
           result))))
