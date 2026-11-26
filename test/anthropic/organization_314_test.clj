(ns anthropic.organization-314-test
  (:require [anthropic.organization :as organization]
            [anthropic.organization-test-support :as support]
            [clojure.test :refer [deftest is]]))

(def ^:private beta-service "com.anthropic.services.blocking.BetaService")
(def ^:private compliance "com.anthropic.services.blocking.beta.ComplianceService")
(def ^:private review "com.anthropic.services.blocking.beta.compliance.ReviewService")
(def ^:private cases "com.anthropic.services.blocking.beta.compliance.review.CaseService")
(def ^:private items "com.anthropic.services.blocking.beta.compliance.review.ItemService")
(def ^:private notices "com.anthropic.services.blocking.beta.compliance.review.NoticeService")
(def ^:private external-storage "com.anthropic.services.blocking.beta.organization.ExternalStorageService")
(def ^:private project-controls "com.anthropic.services.blocking.beta.organization.providers.google.projects.DataControlService")
(def ^:private user-controls "com.anthropic.services.blocking.beta.userprofiles.providers.google.DataControlService")
(def ^:private compliance-settings "com.anthropic.services.blocking.beta.organization.ComplianceSettingService")

(defn- present [optional] (.orElse optional nil))
(defn- client [roots]
  (let [organization (support/proxy-service
                      (Class/forName "com.anthropic.services.blocking.beta.OrganizationService")
                      (fn [method _] (get roots (keyword method))))
        beta (support/proxy-service (Class/forName beta-service)
                                    (fn [method _]
                                      (if (= method "organization") organization
                                          (get roots (keyword method)))))]
    (reify com.anthropic.client.AnthropicClient (beta [_] beta))))
(defn- exercise! [root interfaces leaf response invoke]
  (let [captured (atom nil)
        service (support/proxy-service
                 (Class/forName leaf)
                 (fn [_ args]
                   (reset! captured (or (some #(when (.endsWith (.getName (class %)) "Params") %) args)
                                       (first args)))
                   response))]
    [(invoke (client {root (support/path-service interfaces service)})) @captured]))
(defn- exercise-list! [root interfaces leaf page-class response-class item invoke]
  (let [captured (atom nil) page-service (support/proxy-service (Class/forName leaf) (fn [_ _] nil))
        service (support/leaf-service leaf (fn [[params]] (support/list-page page-class response-class page-service params [item])) captured)]
    [(invoke (client {root (support/path-service interfaces service)})) @captured]))
(defn- model [class-name] (support/sdk-model class-name))
(defn- missing-key [invoke]
  (try (invoke) nil (catch clojure.lang.ExceptionInfo e (select-keys (ex-data e) [:anthropic/error :key]))))
(defn- check-id [result] (is (= "stub" (:id result))))

(deftest list-compliance-review-cases-builds-params-and-round-trips
  (let [[result params] (exercise-list! :compliance [compliance review] cases "com.anthropic.models.beta.compliance.review.cases.CaseListPage" "com.anthropic.models.beta.compliance.review.cases.CaseListPageResponse" (model "com.anthropic.models.beta.compliance.review.cases.BetaComplianceReviewCase") #(organization/list-compliance-review-cases % {:limit 2 :page "next" :betas [:test-beta]}))]
    (is (= 2 (present (.limit params)))) (is (= "next" (present (.page params))))
    (is (= ["test_beta"] (mapv #(.asString %) (present (.betas params))))) (check-id (first result))))
(deftest get-compliance-review-case-builds-params-and-round-trips
  (let [[result params] (exercise! :compliance [compliance review] cases (model "com.anthropic.models.beta.compliance.review.cases.BetaComplianceReviewCase") #(organization/get-compliance-review-case % "case_1" {:betas [:test-beta]}))]
    (is (= "case_1" (present (.caseId params)))) (is (= ["test_beta"] (mapv #(.asString %) (present (.betas params))))) (check-id result)
    (is (= {:anthropic/error :missing-key :key :case-id} (missing-key #(organization/get-compliance-review-case (client {}) nil))))))
(deftest respond-to-compliance-review-case-builds-params-and-round-trips
  (let [[result params] (exercise! :compliance [compliance review] cases (model "com.anthropic.models.beta.compliance.review.cases.BetaComplianceReviewCase") #(organization/respond-to-compliance-review-case % "case_1" {:determination :uphold :betas [:test-beta]}))]
    (is (= "case_1" (present (.caseId params)))) (is (= :uphold (keyword (.asString (.determination (._body params)))))) (is (= ["test_beta"] (mapv #(.asString %) (present (.betas params))))) (check-id result)
    (is (= {:anthropic/error :missing-key :key :case-id} (missing-key #(organization/respond-to-compliance-review-case (client {}) nil {:determination :uphold}))))))
(deftest get-compliance-review-item-builds-params-and-round-trips
  (let [[result params] (exercise! :compliance [compliance review] items (model "com.anthropic.models.beta.compliance.review.items.BetaComplianceReviewItem") #(organization/get-compliance-review-item % "item_1" {:betas [:test-beta]}))]
    (is (= "item_1" (present (.itemId params)))) (is (= ["test_beta"] (mapv #(.asString %) (present (.betas params))))) (check-id result)
    (is (= {:anthropic/error :missing-key :key :item-id} (missing-key #(organization/get-compliance-review-item (client {}) nil))))))
(deftest list-compliance-review-notices-builds-params-and-round-trips
  (let [[result params] (exercise-list! :compliance [compliance review] notices "com.anthropic.models.beta.compliance.review.notices.NoticeListPage" "com.anthropic.models.beta.compliance.review.notices.NoticeListPageResponse" (model "com.anthropic.models.beta.compliance.review.notices.BetaComplianceReviewNotice") #(organization/list-compliance-review-notices % {:limit 2 :betas [:test-beta]}))]
    (is (= 2 (present (.limit params)))) (is (= ["test_beta"] (mapv #(.asString %) (present (.betas params))))) (check-id (first result))))

(defn- external [response invoke] (exercise! :externalStorage [] external-storage response invoke))
(deftest create-external-storage-builds-params-and-round-trips
  (let [[result params] (external (model "com.anthropic.models.beta.organization.externalstorage.BetaExternalStorage") #(organization/create-external-storage % {:provider-config {:type :gcp :bucket-name "bucket" :project-number "123" :region "us-central1"} :display-name "Archive" :betas [:test-beta]}))]
    (is (= "Archive" (present (.displayName params)))) (is (.isGcp (.providerConfig params))) (is (= ["test_beta"] (mapv #(.asString %) (present (.betas params))))) (check-id result)))
(deftest get-external-storage-builds-params-and-round-trips
  (let [[result params] (external (model "com.anthropic.models.beta.organization.externalstorage.BetaExternalStorage") #(organization/get-external-storage % "est_1" {:betas [:test-beta]}))]
    (is (= "est_1" (present (.externalStorageId params)))) (check-id result)
    (is (= {:anthropic/error :missing-key :key :external-storage-id} (missing-key #(organization/get-external-storage (client {}) nil))))))
(deftest update-external-storage-builds-params-and-round-trips
  (let [[result params] (external (model "com.anthropic.models.beta.organization.externalstorage.BetaExternalStorage") #(organization/update-external-storage % "est_1" {:display-name "Archive 2" :betas [:test-beta]}))]
    (is (= "est_1" (present (.externalStorageId params)))) (is (= "Archive 2" (present (.displayName params)))) (check-id result)
    (is (= {:anthropic/error :missing-key :key :external-storage-id} (missing-key #(organization/update-external-storage (client {}) nil {}))))))
(deftest list-external-storage-builds-params-and-round-trips
  (let [[result params] (exercise-list! :externalStorage [] external-storage "com.anthropic.models.beta.organization.externalstorage.ExternalStorageListPage" "com.anthropic.models.beta.organization.externalstorage.ExternalStorageListPageResponse" (model "com.anthropic.models.beta.organization.externalstorage.BetaExternalStorage") #(organization/list-external-storage % {:limit 2 :betas [:test-beta]}))]
    (is (= 2 (present (.limit params)))) (is (= ["test_beta"] (mapv #(.asString %) (present (.betas params))))) (check-id (first result))))
(deftest delete-external-storage-builds-params-and-round-trips
  (let [[result params] (external (model "com.anthropic.models.beta.organization.externalstorage.BetaDeletedExternalStorage") #(organization/delete-external-storage % "est_1"))]
    (is (= "est_1" (present (.externalStorageId params)))) (check-id result)
    (is (= {:anthropic/error :missing-key :key :external-storage-id} (missing-key #(organization/delete-external-storage (client {}) nil))))))
(deftest activate-external-storage-builds-params-and-round-trips
  (let [[result params] (external (model "com.anthropic.models.beta.organization.externalstorage.BetaExternalStorage") #(organization/activate-external-storage % "est_1"))]
    (is (= "est_1" (present (.externalStorageId params)))) (check-id result)
    (is (= {:anthropic/error :missing-key :key :external-storage-id} (missing-key #(organization/activate-external-storage (client {}) nil))))))

(defn- project [response invoke] (exercise! :providers ["com.anthropic.services.blocking.beta.organization.ProviderService" "com.anthropic.services.blocking.beta.organization.providers.GoogleService" "com.anthropic.services.blocking.beta.organization.providers.google.ProjectService"] project-controls response invoke))
(defn- user [response invoke] (exercise! :userProfiles ["com.anthropic.services.blocking.beta.UserProfileService" "com.anthropic.services.blocking.beta.userprofiles.ProviderService" "com.anthropic.services.blocking.beta.userprofiles.providers.GoogleService"] user-controls response invoke))
(deftest get-google-managed-project-data-controls-builds-params-and-round-trips
  (let [[result params] (project (model "com.anthropic.models.beta.organization.providers.google.projects.datacontrols.BetaGoogleManagedProjectDataControls") #(organization/get-google-managed-project-data-controls % "123" {:betas [:test-beta]}))]
    (is (= "123" (present (.projectNumber params)))) (is (= "stub" (:project-number result)))
    (is (= {:anthropic/error :missing-key :key :project-number} (missing-key #(organization/get-google-managed-project-data-controls (client {}) nil))))))
(deftest update-google-managed-project-data-controls-builds-params-and-round-trips
  (let [[result params] (project (model "com.anthropic.models.beta.organization.providers.google.projects.datacontrols.BetaGoogleManagedProjectDataControls") #(organization/update-google-managed-project-data-controls % "123" {:storage {:type :disabled} :betas [:test-beta]}))]
    (is (= "123" (present (.projectNumber params)))) (is (.isDisabled (present (.storage params)))) (is (= "stub" (:project-number result)))
    (is (= {:anthropic/error :missing-key :key :project-number} (missing-key #(organization/update-google-managed-project-data-controls (client {}) nil {}))))))
(deftest get-google-managed-user-profile-data-controls-builds-params-and-round-trips
  (let [[result params] (user (model "com.anthropic.models.beta.userprofiles.providers.google.datacontrols.BetaGoogleManagedDataControls") #(organization/get-google-managed-user-profile-data-controls % "up_1" {:betas [:test-beta]}))]
    (is (= "up_1" (present (.userProfileId params)))) (is (= "stub" (:user-profile-id result)))
    (is (= {:anthropic/error :missing-key :key :user-profile-id} (missing-key #(organization/get-google-managed-user-profile-data-controls (client {}) nil))))))
(deftest update-google-managed-user-profile-data-controls-builds-params-and-round-trips
  (let [[result params] (user (model "com.anthropic.models.beta.userprofiles.providers.google.datacontrols.BetaGoogleManagedDataControls") #(organization/update-google-managed-user-profile-data-controls % "up_1" {:storage {:type :disabled} :betas [:test-beta]}))]
    (is (= "up_1" (present (.userProfileId params)))) (is (.isDisabled (present (.storage params)))) (is (= "stub" (:user-profile-id result)))
    (is (= {:anthropic/error :missing-key :key :user-profile-id} (missing-key #(organization/update-google-managed-user-profile-data-controls (client {}) nil {}))))))

(defn- settings [response invoke] (exercise! :complianceSettings [] compliance-settings response invoke))
(deftest get-beta-compliance-settings-builds-params-and-round-trips
  (let [[result params] (settings (model "com.anthropic.models.beta.organization.compliancesettings.BetaComplianceSettings") #(organization/get-beta-compliance-settings % {:betas [:test-beta]}))]
    (is (= ["test_beta"] (mapv #(.asString %) (present (.betas params))))) (is (keyword? (get-in result [:state :type])))))
(deftest update-beta-compliance-settings-builds-params-and-round-trips
  (let [[result params] (settings (model "com.anthropic.models.beta.organization.compliancesettings.BetaComplianceSettings") #(organization/update-beta-compliance-settings % {:state {:type :enabled} :betas [:test-beta]}))]
    (is (.isEnabled (.state params))) (is (= ["test_beta"] (mapv #(.asString %) (present (.betas params))))) (is (keyword? (get-in result [:state :type])))))
