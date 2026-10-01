(ns anthropic.organization-test-support
  (:import (com.anthropic.client AnthropicClient)
           (com.anthropic.services.blocking BetaService)
           (com.anthropic.services.blocking.beta OrganizationService)
           (com.anthropic.core JsonValue)
           (java.io ByteArrayInputStream)
           (java.lang.reflect InvocationHandler Proxy)
           (java.time LocalDate OffsetDateTime)
           (java.util Optional)))

(defn proxy-service
  "Builds an implementation of interface that sends method name and args to handler."
  [interface handler]
  (Proxy/newProxyInstance (.getClassLoader ^Class interface)
                          (into-array Class [interface])
                          (reify InvocationHandler
                            (invoke [_ _ method args]
                              (handler (.getName method) (vec (or args [])))))))

(defn beta-client
  "Builds a beta client whose organization services are supplied by services map."
  [services]
  (reify AnthropicClient
    (beta [_]
      (reify BetaService
        (organization [_]
          (reify OrganizationService
            (analytics [_] (:analytics services))
            (spendLimits [_] (:spend-limits services))
            (rbacGroups [_] (:rbac-groups services))
            (rbacRoles [_] (:rbac-roles services))
            (plugins [_] (:plugins services))
            (pluginMarketplaces [_] (:plugin-marketplaces services))))))))

(declare complete-builder)

(defn value-for
  "Returns a stub value suitable for the builder field type."
  [^Class type]
  (cond
    (= type String) "stub"
    (or (= type Long) (= type Long/TYPE)) 1
    (or (= type Integer) (= type Integer/TYPE)) 1
    (or (= type Double) (= type Double/TYPE)) 1.0
    (or (= type Float) (= type Float/TYPE)) (float 1.0)
    (or (= type Boolean) (= type Boolean/TYPE)) true
    (= type OffsetDateTime) (OffsetDateTime/parse "2026-09-30T00:00:00Z")
    (= type LocalDate) (LocalDate/parse "2026-09-30")
    (= type java.io.InputStream) (ByteArrayInputStream. (.getBytes "zip"))
    (.isAssignableFrom java.util.List type) []
    (= type JsonValue) (JsonValue/from "stub")
    :else
    (or (try
          (let [builder (.invoke (.getMethod type "builder" (make-array Class 0))
                                 nil (object-array 0))]
            (complete-builder builder))
          (catch Throwable _ nil))
        (try
          (.invoke (.getMethod type "of" (into-array Class [String])) nil
                   (object-array ["stub"]))
          (catch Throwable _ nil)))))

(defn required-field
  "Extracts the missing required builder field from throwable's cause chain."
  [throwable]
  (some->> (iterate #(when % (.getCause ^Throwable %)) throwable)
           (take-while some?)
           (map #(.getMessage ^Throwable %))
           (some #(second (re-find #"`([^`]+)` is required" (or % ""))))))

(defn complete-builder
  "Builds builder, filling required fields with stub values recursively."
  [builder]
  (try
    (.invoke (.getMethod (class builder) "build" (make-array Class 0)) builder
             (object-array 0))
    (catch Throwable error
      (let [field (required-field error)
            candidates (->> (.getMethods (class builder))
                            (filter #(and (= field (.getName ^java.lang.reflect.Method %))
                                          (= 1 (alength (.getParameterTypes ^java.lang.reflect.Method %)))))
                            (sort-by #(let [t (aget (.getParameterTypes ^java.lang.reflect.Method %) 0)]
                                        (cond (= t JsonValue) 2
                                              (= t Optional) 3
                                              :else 0))))]
        (if-let [method (some #(when-let [value (value-for (aget (.getParameterTypes ^java.lang.reflect.Method %) 0))]
                                 [% value]) candidates)]
          (do (.invoke ^java.lang.reflect.Method (first method) builder
                       (object-array [(second method)]))
              (complete-builder builder))
          (throw error))))))

(defn sdk-model
  "Builds a stub SDK model identified by its fully qualified class-name."
  [class-name]
  (let [^Class type (Class/forName class-name)
        builder (.invoke (.getMethod type "builder" (make-array Class 0)) nil (object-array 0))]
    (complete-builder builder)))

(defn list-page
  "Builds a page for page-class from response-class, service, params, and items."
  [page-class response-class service params items]
  (let [response-builder (.invoke (.getMethod (Class/forName response-class) "builder" (make-array Class 0))
                                  nil (object-array 0))
        data (.getMethod (class response-builder) "data" (into-array Class [java.util.List]))
        _ (.invoke data response-builder (object-array [items]))
        response (complete-builder response-builder)
        page-builder (.invoke (.getMethod (Class/forName page-class) "builder" (make-array Class 0))
                              nil (object-array 0))]
    (doseq [[name value] [["service" service] ["params" params] ["response" response]]]
      (clojure.lang.Reflector/invokeInstanceMethod page-builder name (object-array [value])))
    (complete-builder page-builder)))

(defn leaf-service
  "Builds an interface service returning response and storing request args in captured."
  [interface response captured]
  (proxy-service (Class/forName interface)
                 (fn [method args]
                   (when (contains? #{"create" "retrieve" "update" "delete" "set"
                                      "list" "remove" "validateArchive" "validateRepository"} method)
                     (reset! captured (first args)))
                   (if (fn? response) (response args) response))))

(defn path-service
  "Builds nested interface proxies from interfaces leading to leaf."
  [interfaces leaf]
  (if (empty? interfaces)
    leaf
    (proxy-service (Class/forName (first interfaces))
                   (fn [_ _] (path-service (rest interfaces) leaf)))))

(defn exercise-single!
  "Invokes invoke against root's stub service and returns result with captured params."
  [root interface response-class invoke]
  (let [captured (atom nil)
        service (leaf-service interface (sdk-model response-class) captured)]
    [(invoke (beta-client {root service})) @captured]))

(defn exercise-list!
  "Invokes invoke against a nested stub service and returns result with captured params."
  [root interfaces leaf-interface page-class response-class model-class invoke]
  (let [captured (atom nil)
        page-service (proxy-service (Class/forName leaf-interface) (fn [_ _] nil))
        service (leaf-service leaf-interface
                              (fn [[params]]
                                (list-page page-class response-class page-service params
                                           [(sdk-model model-class) (sdk-model model-class)]))
                              captured)
        root-service (path-service interfaces service)]
    [(invoke (beta-client {root root-service})) @captured]))
