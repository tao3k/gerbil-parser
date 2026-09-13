#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Single package test entrypoint; ASP profiles upstream clan discovery.

(import (only-in :clan/poo/object .cc)
        (only-in :asp-gerbil-scheme/build-api
                 +asp-testing-interface+
                 +testing-discovery-profile+
                 testing-interface-add-profile
                 init-profiled-test-environment!))

(def +gerbil-parser-testing-interface+
  (testing-interface-add-profile
   +asp-testing-interface+
   (.cc +testing-discovery-profile+
        ignoreDirectories: '(".data" ".gerbil"))))

(init-profiled-test-environment! +gerbil-parser-testing-interface+)
