; Use PSGML for sgml and xml major modes.
(autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
(autoload 'xml-mode "psgml" "Major mode to edit XML files." t)

;;; Set up file-extension/mode associations.
; Note that I use xml-mode for html... that's because i'm writing 
; XHTML and I want my html to conform to XML.
(setq auto-mode-alist 
      (append '(
		("\\.sgml\\'" . sgml-mode)
		("\\.idd\\'" . sgml-mode)
		("\\.ide\\'" . sgml-mode)
		("\\.htm\\'" . sgml-mode)
		("\\.html\\'" . sgml-mode)
		("\\.xml\\'" . xml-mode)
		("\\.xsl\\'" . xml-mode)
		("\\.fo\\'" . xml-mode)
		("\\.plist\\'" . xml-mode)
		)
	      auto-mode-alist
	      )
      )

; Auto-activate parsing the DTD when a document is loaded.
; If this isn't enabled, syntax coloring won't take affect until
; you manually invoke "DTD->Parse DTD"
(setq sgml-auto-activate-dtd t)

;;; Set up my "DTD->Insert DTD" menu.

(setq sgml-custom-dtd '
      (
       ( "DITA concept"
	 "<?xml version=\"1.0\"?>\n<!DOCTYPE concept SYSTEM \"concept.dtd\">" )
       ( "DITA task"
	 "<?xml version=\"1.0\"?>\n<!DOCTYPE task SYSTEM \"task.dtd\">" )
       ( "DITA reftopic"
	 "<?xml version=\"1.0\"?>\n<!DOCTYPE reftopic SYSTEM \"reftopic.dtd\">" )
       ( "DITA APIdesc"
	 "<?xml version=\"1.0\"?>\n<!DOCTYPE APIdesc SYSTEM \"apidesc.dtd\">" )
       ( "DITA topic"
	 "<?xml version=\"1.0\"?>\n<!DOCTYPE topic SYSTEM \"ditabase.dtd\">" )
       ( "HOD Script"
	 "<?xml version=\"1.0\"?>\n<!DOCTYPE HASCRIPT SYSTEM \"HAScript.dtd\">" )
       ( "XHTML 1.0 Strict"
	 "<?xml version=\"1.0\"?>\n<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"xhtml1-strict.dtd\">" )
       ( "XHTML 1.0 Transitional"
	 "<?xml version=\"1.0\"?>\n<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"xhtml1-transitional.dtd\">" )
       ( "XHTML 1.0 Frameset"
	 "<?xml version=\"1.0\"?>\n<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"xhtml1-frameset.dtd\">" )
       ( "HTML 4.01 Transitional"
	 "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">" )
       ( "HTML 4.01 Strict"
	 "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\">" )
       ( "HTML 4.01 Frameset"
	 "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\">" )
       ( "IBMIDDoc"
	 "<!DOCTYPE ibmiddoc PUBLIC \"+//ISBN 0-933186::IBM//DTD IBMIDDoc//EN\" [\n]>")
       ( "DOCBOOK XML 4.1.2"
	 "<?xml version=\"1.0\"?>\n<!DOCTYPE book PUBLIC \"-//OASIS//DTD DocBook XML V4.1.2//EN\" \"http://www.oasis-open.org/docbook/xml/4.0/docbookx.dtd\" [\n]>")
       )
)

; From Lennart Staflin - re-enabling launch of browser (from original HTML mode)
(defun my-psgml-hook ()
  (local-set-key "\C-c\C-b" 'browse-url-of-buffer)
  )

(add-hook 'sgml-mode-hook 'my-psgml-hook)

;;; Set up Validation support
; First, for sgml-mode, if you always use the same declaration, uncomment
; the following line and set the path to your declaration. If you use 
; more than one SGML declaration, leave it unset and use OpenSP as your
; validator and include DTDDECL entries in your catalog files.
; (setq sgml-declaration "<path to your SGML declaration>")

;(setq sgml-validate-command "onsgmls -s %s %s")
