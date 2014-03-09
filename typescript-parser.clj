;;Presently everything is represeneted via regular expressions
;;This won't work as some things are recursive, So I need
;;To create an ml dataype kind of thing to define the different
;;objects and then convert some regexes into switch-case statements
;;which return an object of my created datatype
(defmacro raise [err] `(throw (Exception. ~(str err))))
(defmacro raise-fmt [format & args]
  `(throw (Exception. (format-string ~format ~@args))))
;;We need an option type, what's the best way to do that?
(ns typescript
  (:use [clojure.string :as string :only (join)])
  (:require [instaparse.core :as yy])
  (:import java.util.regex.Pattern))

;;Various convience functions for constructing regular expressions
(def regex-quote (fn [x] (Pattern/quote x)))
(defn strcat [& args]
  (join args))
(defn strcat-join [seperator & args]
  (join seperator args))
(defn make-re-pattern [& args]
  (re-pattern (join args)))
(defn make-re-union [& args]
  (re-pattern (join "|" args)))
(defn make-seperated-re [re seperator]
  (make-re-pattern "(" re ")" "(" seperator re ")*"))
(defn make-re-list [re]
  (make-seperated-re re (strcat ws-opt "," ws-opt)))

;;So a type is one of, a built-in type, a type query (i.e typeof value),
;;a type-reference, which is a type-name (a qualified identifier), if 
;;the type is a generic type it must be followed by a list of types
;;qualifing it, or a literal type which is one of 
;;an array type (element-type []), 
;;a function type (<types...> (params...) => return-type
;;a constructor type (just a function type with a leading new)
;;or an object type (all other literals are just shorthand for object literals)
;;an object type is really complicated, so I'll explain it elsewhere
(defn ash [integer count]
  (if (>= count 0)
    (bit-shift-left integer count)
    (bit-shift-right integer (- count))))
(defn strtol [str]
  (try
    (java.lang.Long/parseLong  str)
    (catch Exception x nil)))
(declare Type element-type parameter-list string-literal numeric-literal)
;;Most of these variable names are taken from the TypeScript grammer
;;With the exception of whitespace names being shortened
(def identifier
  #"[$_\p{L}][$_\p{L}\p{N}]*")
(def single-line-comment
  #"//.*")
(def multi-line-comment
  #"/\*[\p{M}\P{M}]*?\*/")
;;non-line-terminating-whitespace, but that's quite a long name
(def seperating-whitespace
  #"[\p{Zs}\t\v\xA0\x20\x0C]")
(def line-terminator
  #"[\r\n\u2029\u2028]")
(def predefined-type
  #"any|number|boolean|string|void")
(def public-or-private
  #"public|private")
(def whitespace
  (make-re-pattern "(?:" seperating-whitespace "|" line-terminator "|"
                   single-line-comment "|" multi-line-comment ")"))
(def ws (make-re-pattern whitespace "+"))
(def ws-opt (make-re-pattern whitespace "*"))
(def ws-sep (make-re-pattern "(?:" seperating-whitespace ")"))
(def qualified-identifier
  (make-seperated-re identifier "\\."))
(def identifier-list
  (make-re-list identifier))
(def type-name qualified-identifier)
(def constraint (make-re-pattern "extends" ws Type))
(def type-parameter (make-re-pattern identifier "|" constraint))
(def type-parameters
  (make-re-pattern "<" ws-opt (make-re-list type-parameter) ws-opt ">"))
(def type-arguments 
  (make-re-pattern "<" ws-opt (make-re-list Type) ws-opt ">"))
(def type-query
  (make-re-pattern "typeof" ws qualified-identifier))
(def type-reference 
  (make-re-pattern type-name ws-sep "+" type-arguments "?"))
(def array-type
  (make-re-pattern element-type ws-sep "+" "[" ws-opt "]"))
(def function-type 
  (make-re-pattern (strcat "(" type-parameters ")" "?") ws-opt "("
                   (strcat "(\\(" parameter-list "\\))" "?") ws-opt ")"
                   ws-opt "=>" ws-opt "(" Type ")"))
(def constructor-type
  (make-re-pattern "new" ws-opt function-type))
(def type-annotation (make-re-pattern ":" ws-opt Type))
(def required-parameter
  (make-re-pattern "(" public-or-private "?" ws-opt identifier ws-opt
                   type-annotation "?" ")" "|" 
                   "(" identifier ws-opt string-literal ")"))
(def optional-parameter
  (make-re-pattern public-or-private "?" ws-opt identifier ws-opt 
                   "(" "\\?" ws-opt type-annotation "?" ")" "|"
                   "(" type-annotation "?" ws-opt initaliser ")"))
(def rest-parameter
  (make-re-pattern (re-quote "...") ws-opt identifier ws-opt 
                   type-annotation "?"))
(deftype parameter-list 
    [required-parameters optional-parameter rest-parameter])
(def parameter-list 
  (make-re-pattern "(" (make-re-list required-parameter) ")" "|"
                   "(" (make-re-list optional-parameter) ")" "|"
                   "(" rest-parameter ")" "|"
                   "(" (make-re-list required-parameter) ws-opt
                       (make-re-list optional-parameter) ")" "|"
                   "(" (make-re-list required-parameter) ws-opt
                       rest-parameter")" "|"
                   "(" (make-re-list optional-parameter) ws-opt
                       rest-parameter")" "|"
                   "(" (make-re-list required-parameter) ws-opt
                       (make-re-list optional-parameter) ws-opt
                       rest-parameter")"))
(def call-signature 
  (make-re-pattern "(" type-parameters ")?" ws-opt "\\(" ws-opt
                   parameter-list ws-opt "\\)" ws-opt type-annotation "?"))
(def property-name 
  (make-re-pattern identifier "|" string-literal "|" numeric-literal))
(def property-signature
  (make-re-pattern property-name ws-opt "\\??" ws-opt 
                   "(" type-annotation ")?"))
(def construct-signature
  (make-re-pattern "new" call-signature))
(def index-signature
  (make-re-pattern "(" "\\[" ws-opt identifier ws-opt ":" 
                   ws-opt "string|number" ws-opt "\\]" type-annotation "?" ")"))
(def method-signature 
  (make-re-pattern property-name ws-opt "\\??" ws-opt call-signature))
(def type-member
  (make-re-union property-signature call-signature
                 construct-signature index-signature method-signature))
(def type-body
  (make-seperated-re type-member (strcat ws-opt ";" ws-opt))
(def object-type
  (make-re-pattern "{" ws-opt type-body ws-opt "}"))
(def element-type
  (make-re-union predefined-type type-reference type-query 
                 object-type array-type))
(def type-literal
  (make-re-union object-type array-type function-type constructor-type))
(def Type
  (make-re-union predefined-type type-reference type-query type-literal))
(defn parse-escape [str];assume str starts one character after a \
  (let [[esc & rest] str]
  (letfn [(isdigit [x] (and (> x 0x2F) (< x 0x3A)))
          (strtol-hex 
            ([a b] 
               (if (and (isdigit a) (isdigit b))
                 (+ (bit-shift-left (- a 0x3) 4)
                    (- b 0x30)))
               nil)
            ([a b c d] 
               (if (and (isdigit a) (isdigit b)
                        (isdigit c) (isdigit d))
                 (+ (bit-shift-left (- a 0x3) 12)
                    (bit-shift-left (- b 0x30) 8)
                    (bit-shift-left (- c 0x30) 4)
                    (- d 0x30))
                 nil)))]
    (case esc
    (\' [\' rest])
    (\" [\" rest])
    (\\ [\\ rest])
    (\b [\u0008 rest]);backspace
    (\t [\u0009 rest]);tab
    (\n [\u000a rest]);line-feed
    (\v [\u000b rest]);vtab
    (\f [\u000c rest]);form feed
    (\r [\u000d rest]);carriage return
    (\0 [\u0000 rest])
    (\x (let [[a b & c] rest
              num (strtol-hex a b)]
          (if num [(char num) c]  nil)));maybe raise exception
    (\u (let [[a b c d & e] rest
              num (strtol-hex a b c d)]
          (if num [(char num) e]  nil)));maybe raise exception
    [esc rest]))))
