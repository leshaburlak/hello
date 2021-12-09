(ns hello.core
  (:require [clojure.test :refer [deftest are testing #_thrown-with-msg? is]]
            [clojure.string :as str])
  (:import (java.time LocalDate DayOfWeek)
           (java.time.format DateTimeFormatter)
           (org.apache.commons.validator.routines EmailValidator)))



;; Write a function that takes an Integer and returns it as a string with the correct ordinal indicator suffix (in English). Examples: 1 => 1st, 2 => 2nd.

(defn ordinated [i]
  (when (int? i) ;; otherwise, returns nil
    (let [suffix
          (or
            (and
              (#{11 12 13} (Math/abs (rem i 100)))
              "th")

            ({1 "st" 2 "nd" 3 "rd"} (Math/abs (rem i 10)))

            "th")]
      (str i suffix))))






;; Write a function that takes two dates (date_from, date_to, in dd-mm-yyyy format) and returns the number of Sundays in that range. Example: (‘01-05-2021’, ‘30-05-2021’) => 5.
;; !! assuming incliding both boundaries



(def ^:private f (DateTimeFormatter/ofPattern "dd-MM-yyyy"))


(defn count-sundays
  [from to]
  (when (and (string? from) (string? to)
          (let [r #"\d{2}-\d{2}-\d{4}"]
            (and
              (re-matches r from)
              (re-matches r to))))
    (let [from (LocalDate/parse from f)
          to (LocalDate/parse to f)]
      (cond
        (= from to) (if (=
                          DayOfWeek/SUNDAY
                          (.getDayOfWeek from))
                      1 0)
        ;; ? may throw exception here as well as with wrong inputs
        ;; ? or would a zero be fit ?
        (.isAfter from to) nil
        :else (let [total-days (- (.toEpochDay to)
                                  (.toEpochDay from))
                    whole-weeks (quot total-days 7)
                    rem-days (rem total-days 7)
                    to-weekday-ind (.getValue (.getDayOfWeek to))
                    days-since-last-sunday (if (not= 7 to-weekday-ind)
                                             to-weekday-ind
                                             0)]
                (if (>= rem-days days-since-last-sunday)
                  (inc whole-weeks) whole-weeks))))))





; Mask personal information: create a function that takes a String as input and returns it partly obfuscated.
; The function only recognizes emails and phone numbers, any other String that doesn’t match these types results in an error.

; Emails: emails need to be in a valid email format. To obfuscate it, it should be converted to lowercase and all characters in the
; local-part between the first and last should be replaced by 5 asterisks (*).
; Example: local-part@domain-name.com => l*****t@domain-name.com.

; Phone numbers: a phone number consists of at least 9 digits (0-9) and may contain these two characters (‘ ‘, ‘+’)
; where ‘+’ is only accepted when is the first character.
; To obfuscate it, spaces (‘ ‘) are converted to dashes (‘-’), any digit is converted to an asterisk (‘*’) except for the last 4,
; which remain unchanged and the plus sign (‘+’) also remains unchanged (if present). Example: +44 123 456 789 => +**-***-**6-789.

(defn- email?
  [email]
  (.isValid (EmailValidator/getInstance) email))

(defn- obfuscate-local-part
  [string]
  (str (first string) "*****" (last string)))

(defn- maybe-obfuscate-email
  [string]
  (when (email? string)
    (let [string (str/lower-case string)
          [local-part domain-part] (str/split string #"@")]
      (str (obfuscate-local-part local-part) "@" domain-part))))

(def ^:private phone-simple-re
  #"\+?[\d ]{9,}")

(defn- maybe-phone?
  [string]
  (re-matches phone-simple-re string))



(defn- maybe-obfuscate-phone
  [string]
  (when (maybe-phone? string)
    (let [num-chars #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9}
          {:keys [chs ns-cnt]}
          (->>
            string
            (reverse)
            (reduce
              (fn [{:keys [ns-cnt chs] :as r} ch]
                (cond
                  (num-chars ch) {:ns-cnt (inc ns-cnt)
                                  :chs (cons (if (< ns-cnt 4) ch \*) chs)}
                  (= \space ch) (update r :chs conj \-)
                  (= \+ ch) (update r :chs conj ch)))
              {:ns-cnt 0
               :chs (list)}))]
      (when (>= ns-cnt 9)
        (apply str chs)))))



(defn obfuscate
  [input]
  (or
    (and (string? input)
      (or
        (maybe-obfuscate-email input)
        (maybe-obfuscate-phone input)))

    (throw (Exception. "could not recognize input"))))





