(ns anthropic.http-contract-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [jsonista.core :as json]
            [anthropic.core :as a]
            [anthropic.beta.messages :as beta])
  (:import (com.sun.net.httpserver HttpExchange HttpHandler HttpServer)
           (java.net InetSocketAddress)
           (java.nio.charset StandardCharsets)))

(def json-mapper (json/object-mapper {:decode-key-fn true}))

(defn- request-map [^HttpExchange exchange]
  {:method (.getRequestMethod exchange)
   :path (str (.getRequestURI exchange))
   :headers (into {} (map (fn [[k vs]] [(str/lower-case k) (first vs)]))
                  (.entrySet (.getRequestHeaders exchange)))
   :body (slurp (.getRequestBody exchange))})

(defn- respond! [^HttpExchange exchange status content-type body]
  (let [bytes (.getBytes body StandardCharsets/UTF_8)]
    (.set (.getResponseHeaders exchange) "content-type" content-type)
    (.sendResponseHeaders exchange status (long (if (= 200 status) 0 (count bytes))))
    (with-open [out (.getResponseBody exchange)]
      (.write out bytes))))

(defn- with-server [handler f]
  (let [requests (atom [])
        server (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)]
    (.createContext server "/"
                   (reify HttpHandler
                     (handle [_ exchange]
                       (let [request (request-map exchange)]
                         (swap! requests conj request)
                         (handler exchange request)))))
    (.start server)
    (try
      (f {:base-url (str "http://127.0.0.1:" (.getPort (.getAddress server)))
          :requests requests})
      (finally
        (.stop server 0)))))

(def stable-response
  (json/write-value-as-string
   {:id "msg_contract_stable"
    :type "message"
    :role "assistant"
    :model "claude-sonnet-4-5"
    :content [{:type "text" :text "stable reply"}]
    :stop_reason "end_turn"
    :stop_sequence nil
    :usage {:input_tokens 3 :output_tokens 2}}))

(def beta-response
  (json/write-value-as-string
   {:id "msg_contract_beta"
    :type "message"
    :role "assistant"
    :model "claude-sonnet-4-5"
    :content [{:type "text" :text "beta reply"}]
    :stop_reason "end_turn"
    :stop_sequence nil
    :usage {:input_tokens 4 :output_tokens 2}}))

(defn- message-request []
  {:model "claude-sonnet-4-5"
   :max-tokens 32
   :messages [{:role :user :content "hello"}]})

(deftest stable-messages-create-contract
  (with-server (fn [exchange _]
                 (respond! exchange 200 "application/json" stable-response))
               (fn [{:keys [base-url requests]}]
                 (let [response (a/create-message (a/client {:api-key "test-key" :base-url base-url})
                                                  (message-request))
                       request (first @requests)
                       body (json/read-value (:body request) json-mapper)]
                   (is (= "POST" (:method request)))
                   (is (= "/v1/messages" (:path request)))
                   ;; anthropic-java 3.x sends API keys as a bearer token.
                   (is (= "Bearer test-key" (get-in request [:headers "authorization"])))
                   (is (= "application/json" (get-in request [:headers "content-type"])))
                   (is (= "claude-sonnet-4-5" (:model body)))
                   (is (= 32 (:max_tokens body)))
                   (is (= [{:role "user" :content "hello"}] (:messages body)))
                   (is (= {:id "msg_contract_stable"
                         :model "claude-sonnet-4-5"
                         :role :assistant
                         :stop-reason :end-turn
                         :content [{:type :text :text "stable reply"}]
                         :usage {:input-tokens 3 :output-tokens 2}}
                        response))))))

(deftest beta-messages-create-contract
  (with-server (fn [exchange _]
                 (respond! exchange 200 "application/json" beta-response))
               (fn [{:keys [base-url requests]}]
                 (let [response (beta/create-beta-message
                                 (a/client {:api-key "test-key" :base-url base-url})
                                 (assoc (message-request) :messages [{:role :user :content "beta hello"}]))
                       request (first @requests)
                       body (json/read-value (:body request) json-mapper)]
                   (is (= "POST" (:method request)))
                 ;; anthropic-java 3.3 no longer adds ?beta=true; the beta header marks the route.
                 (is (= "/v1/messages" (:path request)))
                   (is (= "beta hello" (get-in body [:messages 0 :content])))
                   (is (= "beta reply" (get-in response [:content 0 :text])))))))

(deftest models-list-pagination-contract
  (with-server (fn [exchange _]
                 (respond! exchange 200 "application/json"
                           (json/write-value-as-string
                            {:data [{:type "model" :id "model-contract"
                                     :display_name "Contract model"
                                     :created_at "2025-01-01T00:00:00Z"
                                     :lifecycle "active"
                                     :max_input_tokens 100
                                     :max_tokens 20}]
                             :has_more false})))
               (fn [{:keys [base-url requests]}]
                 (let [models (a/list-models (a/client {:api-key "test-key" :base-url base-url})
                                             {:limit 1})
                       request (first @requests)]
                   (is (= "GET" (:method request)))
                   (is (= "/v1/models?limit=1" (:path request)))
                   (is (= [{:id "model-contract"
                          :display-name "Contract model"
                          :created-at "2025-01-01T00:00Z"
                          :lifecycle :active
                          :max-input-tokens 100
                          :max-tokens 20}]
                        models))))))

(deftest sdk-error-json-contract
  (with-server (fn [exchange _]
                 (respond! exchange 400 "application/json"
                           (json/write-value-as-string
                            {:type "error"
                             :error {:type "invalid_request_error"
                                     :message "contract failure"}})))
               (fn [{:keys [base-url]}]
                 (let [error (try
                               (a/create-message (a/client {:api-key "test-key" :base-url base-url})
                                                 (message-request))
                               nil
                               (catch clojure.lang.ExceptionInfo e e))]
                   (is (= :api-error (:anthropic/error (ex-data error))))
                   (is (= :bad-request (:error-type (ex-data error))))
                   (is (= 400 (:status (ex-data error))))
                   (is (= {:type "error"
                           :error {:type "invalid_request_error"
                                   :message "contract failure"}}
                          (:body (ex-data error))))))))

(def sse-response
  (str "event: message_start\n"
       "data: {\"type\":\"message_start\",\"message\":{\"id\":\"msg_stream\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"claude-sonnet-4-5\",\"content\":[],\"stop_reason\":null,\"stop_sequence\":null,\"usage\":{\"input_tokens\":1,\"output_tokens\":0}}}\n\n"
       "event: content_block_start\n"
       "data: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}\n\n"
       "event: content_block_delta\n"
       "data: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"hello \"}}\n\n"
       "event: content_block_delta\n"
       "data: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"stream\"}}\n\n"
       "event: content_block_stop\n"
       "data: {\"type\":\"content_block_stop\",\"index\":0}\n\n"
       "event: message_delta\n"
       "data: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\",\"stop_sequence\":null},\"usage\":{\"output_tokens\":2}}\n\n"
       "event: message_stop\n"
       "data: {\"type\":\"message_stop\"}\n\n"))

(deftest sse-stream-contract
  (with-server (fn [exchange _]
                 (respond! exchange 200 "text/event-stream" sse-response))
               (fn [{:keys [base-url requests]}]
                 (let [events (atom [])]
                   (is (= "hello stream"
                          (a/stream-text (a/client {:api-key "test-key" :base-url base-url})
                                         (message-request)
                                         #(swap! events conj %))))
                   (is (= "POST" (:method (first @requests))))
                   (is (= "text/event-stream" (get-in (first @requests) [:headers "accept"])))
                   (is (= ["hello " "stream"] @events))))))

(defn- numbered-beta-response [n]
  (json/write-value-as-string
   {:id (str "msg_" n)
    :type "message"
    :role "assistant"
    :model "claude-sonnet-4-5"
    :content [{:type "text" :text (str "reply " n)}]
    :stop_reason "end_turn"
    :stop_sequence nil
    :usage {:input_tokens 1 :output_tokens 1}}))

(deftest thread-create-send-append-fork
  (let [counter (atom 0)]
    (with-server (fn [exchange _]
                   (respond! exchange 200 "application/json"
                             (numbered-beta-response (swap! counter inc))))
                 (fn [{:keys [base-url requests]}]
                   (let [client (a/client {:api-key "test-key" :base-url base-url})
                         thread (beta/create-beta-thread client (message-request))
                         first-reply (beta/thread-send thread)
                         _ (is (= "msg_1" (beta/thread-previous-message-id thread)))
                         second-reply (beta/thread-send thread "again")
                         forked (beta/thread-fork thread)
                         fork-reply (beta/thread-send forked {:role :user :content "forked"})
                         _ (beta/thread-append thread {:role :user :content "appended"})
                         third-reply (beta/thread-send thread)
                         bodies (mapv #(json/read-value (:body %) json-mapper) @requests)]
                     (is (= ["msg_1" "msg_2" "msg_3" "msg_4"]
                            (mapv :id [first-reply second-reply fork-reply third-reply])))
                     (is (= "reply 2" (get-in second-reply [:content 0 :text])))
                     ;; The first send opens a server-side thread...
                     (is (= {:type "create"} (:thread (bodies 0))))
                     (is (= [{:role "user" :content "hello"}] (:messages (bodies 0))))
                     ;; ...and the next send carries only the new turn plus the
                     ;; first reply's id, not the whole history.
                     (is (= {:type "continue" :previous_message_id "msg_1"} (:thread (bodies 1))))
                     (is (= [{:role "user" :content "again"}] (:messages (bodies 1))))
                     ;; A fork replays the full local history into a new thread.
                     (is (= {:type "create"} (:thread (bodies 2))))
                     (is (= ["user" "assistant" "user" "assistant" "user"]
                            (mapv :role (:messages (bodies 2)))))
                     (is (= "forked" (:content (last (:messages (bodies 2))))))
                     ;; Appended turns go out with the next send.
                     (is (= {:type "continue" :previous_message_id "msg_2"} (:thread (bodies 3))))
                     (is (= [{:role "user" :content "appended"}] (:messages (bodies 3))))
                     (is (= "msg_4" (:id (beta/thread-last-message thread))))
                     (is (= "msg_4" (beta/thread-previous-message-id thread)))
                     (is (= {:role :user :content "hello"} (first (beta/thread-messages thread))))
                     (is (= [:user :assistant :user :assistant :user :assistant]
                            (mapv :role (beta/thread-messages thread)))))))))

(deftest thread-send-streaming-contract
  (with-server (fn [exchange _]
                 (respond! exchange 200 "text/event-stream" sse-response))
               (fn [{:keys [base-url requests]}]
                 (let [thread (beta/create-beta-thread
                               (a/client {:api-key "test-key" :base-url base-url})
                               (message-request))
                       events (atom [])
                       text (beta/thread-send-streaming thread "stream please" #(swap! events conj %))
                       body (json/read-value (:body (first @requests)) json-mapper)]
                   (is (= "hello stream" text))
                   (is (= :message-start (:type (first @events))))
                   (is (= :message-stop (:type (last @events))))
                   (is (= true (:stream body)))
                   (is (= ["hello" "stream please"] (mapv :content (:messages body))))
                   (is (= "msg_stream" (beta/thread-previous-message-id thread)))
                   (is (= "hello stream" (get-in (beta/thread-last-message thread) [:content 0 :text])))))))

(deftest thread-update-and-unsupported-behavior
  (let [counter (atom 0)]
    (with-server (fn [exchange _]
                   (respond! exchange 200 "application/json"
                             (numbered-beta-response (swap! counter inc))))
                 (fn [{:keys [base-url requests]}]
                   (let [thread (beta/create-beta-thread
                                 (a/client {:api-key "test-key" :base-url base-url})
                                 (assoc (message-request)
                                        :system "be brief"
                                        :temperature 0.5
                                        :unsupported-request-behavior :continue-unthreaded))]
                     (beta/thread-send thread)
                     (beta/thread-update thread {:max-tokens 64 :model "claude-opus-5-5"})
                     (beta/thread-send thread "more")
                     (let [[first-body second-body] (mapv #(json/read-value (:body %) json-mapper) @requests)]
                       (is (= "be brief" (:system first-body)))
                       (is (= 0.5 (:temperature first-body)))
                       (is (= 64 (:max_tokens second-body)))
                       (is (= "claude-opus-5-5" (:model second-body)))
                       (is (= "msg_1" (get-in second-body [:thread :previous_message_id])))))))))

(deftest thread-tool-runner-runs-within-thread
  (let [counter (atom 0)]
    (with-server (fn [exchange _]
                   (respond! exchange 200 "application/json"
                             (numbered-beta-response (swap! counter inc))))
                 (fn [{:keys [base-url requests]}]
                   (let [thread (beta/create-beta-thread
                                 (a/client {:api-key "test-key" :base-url base-url})
                                 (message-request))
                         runner (beta/thread-tool-runner thread)
                         replies (doall ((:messages runner)))
                         body (json/read-value (:body (first @requests)) json-mapper)]
                     (is (= ["msg_1"] (mapv :id replies)))
                     (is (= [{:role "user" :content "hello"}] (:messages body))))))))
