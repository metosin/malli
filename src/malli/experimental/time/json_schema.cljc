(ns malli.experimental.time.json-schema
  (:require [malli.experimental.time]
            [malli.json-schema :as json]))

;; date-time: A string instance is valid against this attribute if it is
;; a valid representation according to the "date-time' ABNF rule
;; (referenced above)

;; date: A string instance is valid against this attribute if it is a
;; valid representation according to the "full-date" ABNF rule
;; (referenced above)

;; time: A string instance is valid against this attribute if it is a
;; valid representation according to the "full-time" ABNF rule
;; (referenced above)

;; duration: A string instance is valid against this attribute if it is
;; a valid representation according to the "duration" ABNF rule
;; (referenced above)

;; Implementations MAY support additional attributes using the other
;; format names defined anywhere in that RFC. If "full-date" or
;; "full-time" are implemented, the corresponding short form ("date" or
;; "time" respectively) MUST be implemented, and MUST behave
;; identically. Implementations SHOULD NOT define extension attributes
;; with any name matching an RFC 3339 format unless it validates
;; according to the rules of that format. There is not currently
;; consensus on the need for supporting all RFC 3339 formats, so this
;; approach of reserving the namespace will encourage experimentation
;; without committing to the entire set. Either the format
;; implementation requirements will become more flexible in general, or
;; these will likely either be promoted to fully specified attributes or
;; dropped.

;; date-fullyear   = 4DIGIT
;; date-month      = 2DIGIT  ; 01-12
;; date-mday       = 2DIGIT  ; 01-28, 01-29, 01-30, 01-31 based on
;;                                         ; month/year
;; time-hour       = 2DIGIT  ; 00-23
;; time-minute     = 2DIGIT  ; 00-59
;; time-second     = 2DIGIT  ; 00-58, 00-59, 00-60 based on leap second
;;                                         ; rules
;; time-secfrac    = "." 1*DIGIT
;; time-numoffset  = ("+" / "-") time-hour ":" time-minute
;; time-offset     = "Z" / time-numoffset

;; partial-time    = time-hour ":" time-minute ":" time-second
;; [time-secfrac]
;; full-date       = date-fullyear "-" date-month "-" date-mday
;; full-time       = partial-time time-offset

;; date-time       = full-date "T" full-time

(defmethod json/accept :time/local-date [_ _ _ _] {:type "string" :format "date"})
(defmethod json/accept :time/offset-time [_ _ _ _] {:type "string" :format "time"})
(defmethod json/accept :time/offset-date-time [_ _ _ _] {:type "string" :format "date-time"})
(defmethod json/accept :time/duration [_ _ _ _] {:type "string" :format "duration"})
