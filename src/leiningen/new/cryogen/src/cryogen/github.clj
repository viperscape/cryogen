(ns cryogen.github
  "pull in github source files and gist content
  malformed api calls, invalid resource uri, and rate-limiting should return nil"
  (:require [cheshire.core :as json]
            [clojure.string :as string])
  (:import (org.apache.commons.codec.binary Base64 StringUtils)))

(defn get-gist [gist-uri]
  (let [gist-id (last (string/split gist-uri #"/+")) ;;just need id for git api
        gist-resp (try (slurp (str "https://api.github.com/gists/" gist-id))
                       (catch Exception e {:error (.getMessage e)}))]

    (when-not (:error gist-resp)
      (if-let [gist (-> (json/parse-string gist-resp)
                        (get "files")
                        first ;;todo: optionally get all gist files?
                        val)]

        {:content (get gist "content")
         :language (get gist "language")
         :name (get gist "filename")
         :id gist-id}))))

(defn parse-linum 
  "pull out just the lines we specified"
  [span content]
  (let [lines (string/split-lines content)]
    (if (second span) ;;if end to span
      (string/join (subvec lines (- (first span) 1) (second span))) ;;get subv vector with span
      (nth lines (- (first span) 1) )))) ;;just get one line

(defn get-src [git-file]
  (let [git-re  (re-find  #"github.com/(.*)/blob/(.+?)/(.+)" git-file) ;;want second and last now (user/repo,file) for git api
        git-res (str "https://api.github.com/repos/" (second git-re) "/contents/" (last git-re))
        git-resp (try (slurp git-res)
                      (catch Exception e {:error (.getMessage e)}))
        linum (string/split (last git-re) #"#L") ;;split at linum
        linum-span (when (second linum) ;;when split, check for span
                     (string/split (second linum) #"-L"))] ;; get start and end for span

    (when-not (:error git-resp)
      (if-let [git-src (json/parse-string git-resp)]
        {:content (let [content (String. (Base64/decodeBase64 (get git-src "content")) "UTF-8")]
                    (if linum-span 
                      (parse-linum (map #(Integer. %) linum-span) content) ;;convert to integers
                      content)) ;;return content if no line numbers specified
         :name (get git-src "name")
         :uri (get (get git-src "_links") "html")}))))


(defn get-gits-ex []
  [(get-gist "https://gist.github.com/viperscape/cec68f0791687f5959f") ;malformed
   (get-gist "https://gist.github.com/viperscape/cec68f0791687f5959f1") ;single gist
   (get-src "https://github.com/viperscape/kuroshio/blob/master/examples/pubsub.clj") ;whole source
   (get-src "https://github.com/lacarmen/cryogen/blob/master/src/leiningen/new/cryogen.clj#L11");source at line
   (get-src "https://github.com/lacarmen/cryogen/blob/master/src/leiningen/new/cryogen.clj#L11-L14")]) ;source at line-span

;(prn (get-gits-ex))
