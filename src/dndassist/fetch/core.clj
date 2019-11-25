(ns dndassist.fetch.core
  (:require [clojure.data.json :as json])
  (:gen-class)
  (:import (com.gargoylesoftware.htmlunit WebClient BrowserVersion)
           (com.gargoylesoftware.htmlunit.html HtmlPage HtmlElement HtmlForm)))

(defn login-client
  "Create a WebClient and log it in to DNDBeyond"
  [username password]
  (let [client (WebClient. BrowserVersion/CHROME)]
    (-> (.getCookieManager client) (.setCookiesEnabled true))
    (let [login-page (->
                       (.getPage client "https://www.dndbeyond.com/login")
                       (.getHtmlElementById "signin-with-twitch")
                       .click)
          login-form (.getFirstByXPath login-page "//form[@id='loginForm']")]
      (-> (.getInputByName login-form "username") (.setValueAttribute username))
      (-> (.getInputByName login-form"password") (.setValueAttribute password))
      (println "ADDED INPUTS! PRECLICK PRINOUT")
      (-> login-page .getWebResponse .getContentAsString println)
      (let [auth-page (-> (.getElementsByTagName login-form "button") (.get 0) .click)]
        (println "POST CLICK PRINOUT")
        (-> auth-page .getWebResponse .getContentAsString println))
      (println "DONE PRINOUT!")
      )
    client
    ))

