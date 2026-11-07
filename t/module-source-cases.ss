;;; Character offsets; generators translate these witnesses to Rust byte offsets.
(export module-source-cases)
(def module-source-cases
 '(("preamble\n---- MODULE Demo ----" 0 9)
   ("---- MODULE A ----\n====\nfooter" 0 #f)
   ("---- MODULE A ----\n====\nfooter" 23 30)
   ("---- MODULE A ----\n====\nfooter" 24 30)
   ("---- MODULE 字 ----\n====\nλ" 24 25)
   ("---- MODULE A ----\n---- MODULE B ----\n====\ninner\n====\nfooter" 43 #f)
   ("---- MODULE A ----\n---- MODULE B ----\n====\ninner\n====\nfooter" 54 60)
   ("---- MODULE A ----\n\"====\"\n====\nfooter" 25 #f)
   ("---- MODULE A ----\n====\nfooter\n---- MODULE B ----" 24 31)
   ("preamble" 0 8)
   ("preamble" 8 #f)))
