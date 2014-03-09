(defmacro raise [err] `(throw (Exception. ~(str err))))
(defmacro raise-fmt [format & args]
  `(throw (Exception. (format-string ~format ~@args))))
;;We need an option type, what's the best way to do that?
(ns typescript
  (:require [instaparse.core :as yy]
            [clojure.core.typed :as typed]))
(def ts-parser (yy/parser (slurp "typescript.ebnf")))
(defn ts-parse [input] (yy/parse ts-parser input))
(defmacro eq [a b] (identical? a b))
(defn ash [integer count]
  (if (>= count 0)
    (bit-shift-left integer count)
    (bit-shift-right integer (- count))))
(defn strtol [str]
  (try
    (java.lang.Long/parseLong str)
    (catch Exception x nil)))
(defn strtod [str]
  (try
    (java.lang.Double/parseDouble str)
    (catch Exception x nil)))
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
;;I really wish closure had multiline comments
#_"Well this'll work as a comment I guess, I really wish I could extend the 
clojure reader without having to hack the java source.

Anyway, here's my plan for representing typescript types, lets
just do this in c syntax and translate to closure maps later
struct Type {
  enum type_kind {
    type_predefined,
    type_reference,
    type_query,
    type_literal,
  }
  union type {
    symbol builtin;
    symbol ref;
    object query;
    type_literal *literal;
  }
}
struct type_literal {
  enum literal_kind {
    literal_array,
    literal_function,
    literal_constructor,
    literal_object,
  }
  union literal {
  ...you know what goes here;
  //except
    type element-type;//for arrays, no need to use a seperate structure
  }
}
struct function_type {//same thing for constructor type
  int req-args;
  int opt-args;
  bool rest_arg;
  type ret-type;
  type *arg_types;
}
struct object-type {
  int num_members;
  member_type *member_types;
}
//need to read what the semantics of object type literals are
"
(defn make-fun-type [req-args opt-args rest-arg ret-type & arg-types]
  {:req-args req-args
   :opt-args opt-args
   :rest-arg rest-arg
   :ret-type ret-type
   :arg-types arg-types})
(defmulti parse-predefined-type (fn [key & rest] key))
(defmethod parse-predefined-type "any" [key & rest]
  'Any)
(defmethod parse-predefined-type "number" [key & rest]
  ;;All javascript numbers are doubles
  java.lang.Double)
(defmethod parse-predefined-type "string" [key & rest]
  java.lang.String)
(defmethod parse-predefined-type "boolean" [key & rest]
  java.lang.Boolean)
(defmethod parse-predefined-type "void" [key & rest]
  nil)

