;; Setup completion style which is responsible for candidate filtering
(setq completion-styles '(orderless))

;; Enable Selectrum, Prescient and Marginalia
(selectrum-mode)
;; Disable filtering as orderless is used for that
(setq selectrum-prescient-enable-filtering nil)
(selectrum-prescient-mode)
  (setq selectrum-fix-vertical-window-height 17
        selectrum-max-window-height 17)

(marginalia-mode)

(defun flex-if-twiddle (pattern _index _total)
  (when (string-suffix-p "~" pattern)
    `(orderless-flex . ,(substring pattern 0 -1))))

(defun first-initialism (pattern index _total)
  (if (= index 0) 'orderless-initialism))

(defun without-if-bang (pattern _index _total)
  "Define a '!not' exclusion prefix for literal strings."
  (when (string-prefix-p "!" pattern)
    `(orderless-without-literal . ,(substring pattern 1))))

(setq orderless-matching-styles '(orderless-regexp)
      orderless-style-dispatchers '(flex-if-twiddle
                                    without-if-bang))

;; Use completing-read prompter and set binding for Embark context menu
;(setq embark-prompter #'embark-completing-read-prompter)
(setq embark-action-indicator
      (lambda (map _target)
        (which-key--show-keymap "Embark" map nil nil 'no-paging)
        #'which-key--hide-popup-ignore-command)
      embark-become-indicator embark-action-indicator)
(global-set-key (kbd "C-o") #'embark-act)

;; Keybindings for Consult
(global-set-key (kbd "C-x b") #'consult-buffer)
