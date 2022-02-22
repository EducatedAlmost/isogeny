(ns ae.isogeny-test
  (:require [clojure.test :as t]
            [cheshire.core :as json]
            [ae.isogeny :as sut]))

(t/deftest deep-merge-test
  (t/testing "Maps can be deep-merged."
    (let [m1 {:foo {:bar :bar :qux :qux}}
          m2 {:foo {:bar :baz}}
          expected {:foo {:bar :baz :qux :qux}}
          actual (sut/deep-merge m1 m2)]
      (t/is (= expected actual))))
  (t/testing "Nil maps are ignored."
    (let [m1 {:foo :bar}
          m2 nil
          expected {:foo :bar}
          actual (sut/deep-merge m1 m2)]
      (t/is (= expected actual)))))

(t/deftest ->outputs-test
  (t/testing "Proper outputs are unchanged."
    (let [o1 "foo" o2 "-" o3 nil
          expected [o1 *out* *out*]
          actual (sut/->outputs [o1 o2 o3])]
      (t/is (= expected actual))))
  (t/testing "Proper outputs are unchanged."
    (let [o "foo"
          expected [o]
          actual (sut/->outputs [o])]
      (t/is (= expected actual))))
  (t/testing "Hyphens redirect to *out*."
    (let [o "-"
          expected [*out*]
          actual (sut/->outputs [o])]
      (t/is (= expected actual))))
  (t/testing "Missing outputs redirect to *out*."
    (let [o nil
          expected [*out*]
          actual (sut/->outputs [o])]
      (t/is (= expected actual)))))

(t/deftest ->standard-test
  (t/testing ""
    (let [template-files ["file.foo" "other.file.bar"]
          expected ["file" "other.file"]
          actual (sut/->standard template-files)]
      (t/is (= expected actual)))))

(t/deftest render-test
  (t/testing "The context will be preferred to its default."
    (let [templates ["{{foo}}"]
          context (str {:foo "bar"})
          context-default (str {:foo "baz"})
          output "output.txt"
          options {::sut/outputs [output]}
          expected [{::sut/file output ::sut/content "bar"}]
          actual (sut/render templates context context-default options)]
      (t/is (= expected actual))))
  (t/testing "Default context will be used if the context is nil."
    (let [templates ["{{foo}}"]
          context nil
          context-default (str {:foo "bar"})
          output "output.txt"
          options {::sut/outputs [output]}
          expected [{::sut/file output ::sut/content "bar"}]
          actual (sut/render templates context context-default options)]
      (t/is (= expected actual))))
  (t/testing "A present context can be overridden."
    (let [templates ["{{foo}}\n{{fee}}"]
          context (str {:foo "baz" :fee "qux"})
          context-default nil
          output "output.txt"
          options {::sut/outputs [output] ::sut/context-override (str {:foo "bar"})}
          expected [{::sut/file output ::sut/content "bar\nqux"}]
          actual (sut/render templates context context-default options)]
      (t/is (= expected actual))))
  (t/testing "A nil context can be overridden."
    (let [templates ["{{foo}}"]
          context nil
          context-default nil
          output "output.txt"
          options {::sut/outputs [output] ::sut/context-override (str {:foo "bar"})}
          expected [{::sut/file output ::sut/content "bar"}]
          actual (sut/render templates context context-default options)]
      (t/is (= expected actual))))
  (t/testing "Interior values can be used in the template."
    (let [templates ["{{foo.bar}}"]
          context (str {:foo {:bar "baz"}})
          context-default nil
          output "output.txt"
          options {::sut/outputs [output]}
          expected [{::sut/file output ::sut/content "baz"}]
          actual (sut/render templates context context-default options)]
      (t/is (= expected actual))))
  (t/testing "JSON can be used to provide context."
    (let [templates ["{{foo}}"]
          context (json/generate-string {:foo "baz"})
          context-default nil
          output "output.txt"
          options {::sut/outputs [output] ::sut/json? true}
          expected [{::sut/file output ::sut/content "baz"}]
          actual (sut/render templates context context-default options)]
      (t/is (= expected actual))))
  (t/testing "Contexts can be deep-merged."
    (let [templates ["{{foo.one}} {{foo.two}} {{foo.three}}"]
          context (str {:foo {:one "one" :two "two"}})
          context-default nil
          output "output.txt"
          options {::sut/outputs [output]
                   ::sut/context-override (str {:foo {:two "two" :three "three"}})
                   ::sut/deep-merge? true}
          expected [{::sut/file output ::sut/content "one two three"}]
          actual (sut/render templates context context-default options)]
      (t/is (= expected actual))))
  (t/testing "Context will be evaled before substitution."
    (let [templates ["{{foo}}\n{{bar}}"]
          context (str `{:foo (+ 1 2) :bar "baz"})
          context-default nil
          output "output.txt"
          options {::sut/outputs [output] ::sut/deep-merge? true}
          expected [{::sut/file output ::sut/content "3\nbaz"}]
          actual (sut/render templates context context-default options)]
      (t/is (= expected actual))))
  (t/testing "Standard option will override outputs."
    (let [template-files ["output.template"]
          templates ["{{foo}}"]
          context (str {:foo "bar"})
          context-default nil
          output "output"
          options {::sut/template-files template-files
                   ::sut/outputs ["incorrect.file"]
                   ::sut/deep-merge? true
                   ::sut/standard? true}
          expected [{::sut/file output ::sut/content "bar"}]
          actual (sut/render templates context context-default options)]
      (t/is (= expected actual)))))

(t/deftest prepare-test
  (t/testing "Config files may be prepared for use with Isogeny."
    (let [file "config" content "content"
          expected [{::sut/file "config.template" ::sut/content content}
                    {::sut/file "config.HOST.edn" ::sut/content "{}"}]
          actual (sut/prepare {::sut/file file ::sut/content content} false)]
      (t/is (= expected actual)))))
