(ns hello.core-test
  (:require [clojure.test :refer :all]
            [hello.core :refer :all])
  (:import (java.time.format DateTimeFormatter)
           (java.time LocalDate)))




(deftest ordinated-test
  (are [x y] (= x (ordinated y))
    nil ""
    "-4th" -4
    "-1st" -1
    "0th" 0
    "1st" 1
    "2nd" 2
    "3rd" 3
    "4th" 4
    "9th" 9
    "10th" 10
    "11th" 11
    "12th" 12
    "13th" 13
    "14th" 14
    "20th" 20
    "21st" 21
    "22nd" 22
    "23rd" 23
    "24th" 24
    "100th" 100
    "101st" 101
    "111th" 111
    "121st" 121
    "125th" 125))


(deftest count-sundays-test
  (let [f (DateTimeFormatter/ofPattern "dd-MM-yyyy")
        ; two sundays one week apart
        from-d (LocalDate/parse "05-12-2021" f)
        to-d (LocalDate/parse "12-12-2021" f)
        ; from-d + 1 day = monday
        mon-d (.plusDays from-d 1)

        ;; example dates
        efrom-d (LocalDate/parse "01-05-2021" f)
        eto-d (LocalDate/parse "30-05-2021" f)

        ;; really-far-apart-dates
        ancient-from (LocalDate/parse "01-05-0001" f)
        future-to (LocalDate/parse "01-05-9999" f)

        ]
    (testing "various dates"
      (are [expected-count from to] (= expected-count (count-sundays (.format from f) (.format to f)))
        ;; example dates
        5 efrom-d eto-d
        ;; two sundays one week apart
        2 from-d to-d
        ;; same sunday as both `from` and `to`
        1 from-d from-d
        ;; same monday as both `from` and `to`
        0 mon-d mon-d
        ;; two sundays where `to` is before `from`
        nil to-d from-d
        ;; really far apart dates
        521670 ancient-from future-to))
    (testing "junk args"
      (are [expected-result from to] (= expected-result (count-sundays from to))
        ;; some junk strings
        nil "asdasdas" "2021-01-01"
        ;; something else
        nil 1 :keyword))))




(deftest obfuscate-test
  (testing "ok inputs"
    (are [expected input] (= expected (obfuscate input))
      "+**-***-**6-789" "+44 123 456 789"
      "*******6789" "44123456789"
      "+*-***-***-67-09" "+7 925 175 67 09"
      "****-***-6709" "7925 175 6709"
      "****-***---------6709" "7925 175         6709" ;; ?


      "l*****t@domain-name.com" "local-part@domain-name.com"
      "x*****x@ya.ru" "X@yA.rU"
      "d*****l@example.com" "disposable.style.email.with+symbol@example.com"
      "1*****3@example.com" "123@example.com"))
  (testing "nok inputs"
    ;; empty str
    (is (thrown-with-msg? Exception #"could not recognize input"
          (obfuscate "")))
    ;; nil
    (is (thrown-with-msg? Exception #"could not recognize input"
          (obfuscate nil)))

    ;; phones

    ;; not enough digits
    (is (thrown-with-msg? Exception #"could not recognize input"
          (obfuscate "+7             999")))
    ;; dashes instead of spaces
    (is (thrown-with-msg? Exception #"could not recognize input"
          (obfuscate "+7-925-175-67-99")))
    ;; more than one +
    (is (thrown-with-msg? Exception #"could not recognize input"
          (obfuscate "++7 925 175 67 99")))
    ;; contains a letter
    (is (thrown-with-msg? Exception #"could not recognize input"
          (obfuscate "+7 925 a75 67 99")))
    ;; contains a @
    (is (thrown-with-msg? Exception #"could not recognize input"
          (obfuscate "+7 925 @75 67 99")))
    ;; plus is somewhere in the middle
    (is (thrown-with-msg? Exception #"could not recognize input"
          (obfuscate "77+7 925 175 67 99")))
    ;; plus is at the end
    (is (thrown-with-msg? Exception #"could not recognize input"
          (obfuscate "777 925 175 67 99+")))

    ;;;;;;;;;;;;;;;;;
    ;; emails

    ;; more than one @
    (is (thrown-with-msg? Exception #"could not recognize input"
          (obfuscate "example@example@com")))
    ;; no @
    (is (thrown-with-msg? Exception #"could not recognize input"
          (obfuscate "example.example.com")))
    ;; underscore in domain part
    (is (thrown-with-msg? Exception #"could not recognize input"
          (obfuscate "example@hello_there.com")))))
