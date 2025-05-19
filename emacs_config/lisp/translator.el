;;; translator.el --- Prosty tłumacz Google Translate lub translate-shell -*- lexical-binding: t; -*-

;; Wymaga: sudo apt install translate-shell
;; Arch Linux: yay -S translate-shell

(defun translator--run-command (text lang)
  "Uruchamia translate-shell na podanym tekście do danego języka LANG."
  (let* ((command (format "trans -b :%s \"%s\"" lang text)))
    (shell-command-to-string command)))

(defun translator--get-region-or-error ()
  "Zwraca zaznaczony tekst lub zgłasza błąd, jeśli nic nie zaznaczono."
  (unless (use-region-p)
    (error "Zaznacz tekst do przetłumaczenia"))
  (buffer-substring-no-properties (region-beginning) (region-end)))

;;;###autoload
(defun translator/translate-region ()
  "Tłumaczy zaznaczony tekst z polskiego na angielski i pokazuje wynik w minibufferze."
  (interactive)
  (message "Tłumaczenie: %s"
           (translator--run-command (translator--get-region-or-error) "en")))

;;;###autoload
(defun translator/translate-region-en-to-pl ()
  "Tłumaczy zaznaczony tekst z angielskiego na polski i pokazuje wynik w minibufferze."
  (interactive)
  (message "Tłumaczenie: %s"
           (translator--run-command (translator--get-region-or-error) "pl")))

;;;###autoload
(defun translator/translate-buffer-to-english ()
  "Tłumaczy cały bufor z polskiego na angielski i wyświetla wynik w nowym buforze."
  (interactive)
  (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
         (translated (translator--run-command text "en")))
    (with-output-to-temp-buffer "*Translation*"
      (princ translated))))

(provide 'translator)
;;; translator.el ends here
