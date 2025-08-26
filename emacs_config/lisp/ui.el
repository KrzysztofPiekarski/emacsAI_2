;;; ui.el --- Professional User Interface & Visual Enhancements -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Krispi
;; Author: Krispi
;; Keywords: emacs, ui, theme, visual, dashboard
;; Description: Professional user interface with modern themes and visual enhancements
;; Version: 2.0.0

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:
;; This module provides a professional and beautiful user interface:
;; - Modern theme system with Catppuccin
;; - Enhanced modeline with Doom modeline
;; - Professional dashboard for startup
;; - Custom font configuration
;; - Visual enhancements and customizations
;;
;; Features:
;; - Beautiful Catppuccin theme with multiple flavors
;; - Professional modeline with time, battery, and weather
;; - Modern dashboard with recent files and projects
;; - Custom font setup with FiraCode Nerd Font
;; - Enhanced visual appearance and user experience

;;; Code:

;; ============================================================================
;; 🎨 THEME SYSTEM - MODERN COLOR SCHEMES
;; ============================================================================

;; === CATPPUCCIN THEME - BEAUTIFUL COLOR PALETTE ===
;; Catppuccin provides a beautiful, modern color scheme
;; with multiple flavor options for different preferences
(use-package catppuccin-theme
  :ensure t
  :config
  (setq catppuccin-flavor 'mocha) ;; Available: 'latte, 'frappe, 'macchiato, 'mocha
  (load-theme 'catppuccin :no-confirm)) 
  
;; ============================================================================
;; 📊 MODELINE - ENHANCED STATUS BAR
;; ============================================================================

;; === DOOM MODELINE - PROFESSIONAL STATUS BAR ===
;; Doom modeline provides a modern, information-rich status bar
;; with time, battery, weather, and enhanced buffer information
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 35)                    ;; Modeline height - powiększona
  (doom-modeline-bar-width 5)                  ;; Bar width - szerszy pasek
  (doom-modeline-time t)                       ;; Show time
  (doom-modeline-battery t)                    ;; Show battery status
  (doom-modeline-buffer-file-name-style 'truncate-except-project) ;; Buffer name style
  (doom-modeline-icon t)                       ;; Show icons
  (doom-modeline-major-mode-icon t)            ;; Show major mode icon
  (doom-modeline-major-mode-color-icon t)      ;; Colorful major mode icon
  (doom-modeline-buffer-state-icon t)          ;; Show buffer state icon
  (doom-modeline-buffer-modification-icon t)   ;; Show modification icon
  (doom-modeline-unicode-fallback t)           ;; Unicode fallback
  (doom-modeline-minor-modes nil)              ;; Hide minor modes for cleaner look
  (doom-modeline-enable-word-count nil)        ;; Disable word count
  (doom-modeline-buffer-encoding t)            ;; Show encoding
  (doom-modeline-indent-info nil)              ;; Hide indent info
  (doom-modeline-checker-simple-format t)      ;; Simple checker format
  (doom-modeline-vcs-max-length 15)            ;; VCS branch name length
  (doom-modeline-env-version t)                ;; Show environment version
  (doom-modeline-irc-stylize 'identity)        ;; IRC styling
  (doom-modeline-github-interval 1800))        ;; GitHub update interval

;; === MODELINE FONT - ENHANCED READABILITY ===
;; Increase modeline font size for better readability
(set-face-attribute 'mode-line nil :height 1.1)
(set-face-attribute 'mode-line-inactive nil :height 1.1)

;; ============================================================================
;; 🔤 FONT CONFIGURATION - PROFESSIONAL TYPOGRAPHY
;; ============================================================================

;; === CUSTOM FONT SETUP ===
;; Configure custom fonts for enhanced readability and appearance
;; FiraCode Nerd Font provides beautiful programming ligatures
(set-face-attribute 'default nil :family "FiraCode Nerd Font" :height 120)
 
;; ============================================================================
;; 🏠 DASHBOARD - PROFESSIONAL STARTUP INTERFACE
;; ============================================================================

;; === DASHBOARD - MODERN STARTUP SCREEN ===
;; Dashboard provides a beautiful and informative startup screen
;; with recent files, bookmarks, projects, and agenda items
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Witaj w Emacsie!") ;; Welcome message
  (setq dashboard-startup-banner 'official)              ;; Official Emacs banner
  (setq dashboard-items '((recents  . 5)                 ;; Recent files
                          (bookmarks . 5)                ;; Bookmarks
                          (projects . 5)                 ;; Projects
                          (agenda . 5)))                 ;; Agenda items
  (setq dashboard-set-footer nil)                        ;; No footer
  (setq dashboard-set-init-info t)                       ;; Show init info
  (setq dashboard-set-heading-icons t)                   ;; Show heading icons
  (setq dashboard-set-file-icons t))                     ;; Show file icons
 
;; === DASHBOARD ENHANCEMENTS ===
;; Customize dashboard appearance and behavior
(add-hook 'dashboard-mode-hook
          (lambda ()
            (face-remap-add-relative 'default :height 1.2))) ;; Increase font size

;; ============================================================================
;; 🌈 VISUAL ENHANCEMENTS - BEAUTIFUL CODE DISPLAY
;; ============================================================================

;; === RAINBOW DELIMITERS - COLORFUL PARENTHESES ===
;; Rainbow delimiters provide colorful parentheses for better code readability
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  ;; Enhanced rainbow colors for better visibility
  (set-face-foreground 'rainbow-delimiters-depth-1-face "#ff6c6b")
  (set-face-foreground 'rainbow-delimiters-depth-2-face "#98be65")
  (set-face-foreground 'rainbow-delimiters-depth-3-face "#da8548")
  (set-face-foreground 'rainbow-delimiters-depth-4-face "#51afef")
  (set-face-foreground 'rainbow-delimiters-depth-5-face "#c678dd")
  (set-face-foreground 'rainbow-delimiters-depth-6-face "#46d9ff")
  (set-face-foreground 'rainbow-delimiters-depth-7-face "#a9a1e1")
  (set-face-foreground 'rainbow-delimiters-depth-8-face "#f09383")
  (set-face-foreground 'rainbow-delimiters-depth-9-face "#5b6268"))

;; === BEACON MODE - CURSOR LOCATION HIGHLIGHT ===
;; Beacon briefly highlights the cursor whenever the window scrolls
(use-package beacon
  :ensure t
  :init (beacon-mode 1)
  :custom
  (beacon-size 40)                    ;; Size of the beacon
  (beacon-blink-when-buffer-changes t) ;; Blink when switching buffers
  (beacon-blink-when-window-scrolls t) ;; Blink when scrolling
  (beacon-blink-when-window-changes t) ;; Blink when changing windows
  (beacon-blink-when-point-moves-vertically nil) ;; Disable vertical movement blinking
  (beacon-blink-duration 0.3)         ;; Duration of blink
  (beacon-blink-delay 0.3)            ;; Delay before blink
  (beacon-color "#ff6c6b"))           ;; Beacon color

;; === SOLAIRE MODE - DISTINGUISH BUFFERS ===
;; Solaire mode makes "real" buffers brighter than "unreal" buffers
(use-package solaire-mode
  :ensure t
  :config
  ;; Enable solaire mode globally
  (solaire-global-mode +1)
  
  ;; Basic configuration that works across versions
  (with-eval-after-load 'solaire-mode
    ;; Try to enable in minibuffer if function exists
    (when (fboundp 'solaire-mode-in-minibuffer)
      (condition-case nil
          (solaire-mode-in-minibuffer)
        (error nil)))
    
    ;; Alternative minibuffer setup
    (add-hook 'minibuffer-setup-hook 
              (lambda ()
                (when (fboundp 'solaire-mode)
                  (solaire-mode 1))))
    
    ;; Ensure file buffers are brightened
    (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
    
    ;; Handle ediff buffers
    (add-hook 'ediff-prepare-buffer-hook 
              (lambda ()
                (when (fboundp 'solaire-mode)
                  (solaire-mode 1))))))

;; === SMOOTH SCROLLING - ENHANCED NAVIGATION ===
;; Smooth scrolling provides better visual feedback during navigation
(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode 1)
  (setq smooth-scroll-margin 5)
  (setq scroll-step 1)
  (setq scroll-conservatively 10000)
  (setq auto-window-vscroll nil))

;; === PRETTIFY SYMBOLS - BEAUTIFUL LIGATURES ===
;; Prettify symbols mode displays beautiful ligatures for programming
(use-package prettify-symbols-mode
  :hook (prog-mode . prettify-symbols-mode)
  :config
  ;; Add custom symbols for various programming languages
  (defun my/add-pretty-symbols ()
    "Add custom prettify symbols for programming languages."
    (setq prettify-symbols-alist
          '(("lambda" . 955)      ;; λ
            ("->" . 8594)         ;; →
            ("=>" . 8658)         ;; ⇒
            ("!=" . 8800)         ;; ≠
            ("<=" . 8804)         ;; ≤
            (">=" . 8805)         ;; ≥
            ("&&" . 8743)         ;; ∧
            ("||" . 8744)         ;; ∨
            ("infinity" . 8734)   ;; ∞
            ("sum" . 8721)        ;; ∑
            ("sqrt" . 8730)       ;; √
            ("alpha" . 945)       ;; α
            ("beta" . 946)        ;; β
            ("gamma" . 947)       ;; γ
            ("delta" . 948)       ;; δ
            ("epsilon" . 949)     ;; ε
            ("pi" . 960))))       ;; π
  (add-hook 'prog-mode-hook #'my/add-pretty-symbols))

;; === HIGHLIGHT INDENT GUIDES - VISUAL INDENTATION ===
;; Highlight indent guides shows visual indentation lines
(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-character ?\|)
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-auto-enabled nil)
  :config
  (set-face-background 'highlight-indent-guides-odd-face "darkgray")
  (set-face-background 'highlight-indent-guides-even-face "dimgray")
  (set-face-foreground 'highlight-indent-guides-character-face "gray"))

;; ============================================================================
;; 🎯 MODELINE CUSTOMIZATION - ENHANCED APPEARANCE
;; ============================================================================

;; === MODELINE FONT CUSTOMIZATION ===
;; Customize modeline fonts for better appearance and consistency
;; This ensures the modeline uses the same beautiful font as the rest of the interface
(custom-set-faces
 '(mode-line ((t (:family "FiraCode Nerd Font" :height 1.1 :weight normal))))      ;; Active modeline
 '(mode-line-inactive ((t (:family "FiraCode Nerd Font" :height 1.2)))))           ;; Inactive modeline

;; ============================================================================
;; ✨ ADVANCED VISUAL EFFECTS - PREMIUM EXPERIENCE
;; ============================================================================

;; === CENTAUR TABS - BEAUTIFUL TAB BAR ===
;; Centaur tabs provides beautiful, modern tab interface
(use-package centaur-tabs
  :ensure t
  :demand
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)
  (centaur-tabs-group-by-projectile-project)
  :custom
  (centaur-tabs-style "wave")              ;; Tab style: bar, wave, rounded, slant
  (centaur-tabs-height 32)                 ;; Tab height
  (centaur-tabs-set-icons t)               ;; Show icons
  (centaur-tabs-show-count t)              ;; Show tab count
  (centaur-tabs-set-bar 'left)             ;; Bar position
  (centaur-tabs-set-close-button t)        ;; Show close button
  (centaur-tabs-gray-out-icons 'buffer)    ;; Gray out inactive icons
  (centaur-tabs-cycle-scope 'tabs)         ;; Cycle through tabs
  :bind
  ("C-<prior>" . centaur-tabs-backward)    ;; Previous tab
  ("C-<next>" . centaur-tabs-forward)      ;; Next tab
  ("C-c t c" . centaur-tabs-counsel-switch-group)) ;; Switch tab group

;; === DIMMER - FOCUS ON ACTIVE WINDOW ===
;; Dimmer dims inactive buffers to focus on the active one
(use-package dimmer
  :ensure t
  :custom
  (dimmer-fraction 0.3)                    ;; Dimming strength
  (dimmer-exclusion-regexp-list            ;; Don't dim these buffers
   '(".*Minibuf.*" ".*which-key.*" ".*NeoTree.*" ".*Messages.*" ".*Async.*"))
  :config
  (dimmer-configure-which-key)
  (dimmer-configure-helm)
  (dimmer-mode t))

;; === WHICH KEY - BEAUTIFUL KEY HINTS ===
;; Enhanced which-key configuration for better visual appearance
(use-package which-key
  :ensure t
  :config
  (which-key-mode 1)
  :custom
  (which-key-popup-type 'side-window)      ;; Popup type
  (which-key-side-window-location 'bottom) ;; Location
  (which-key-side-window-max-height 0.25)  ;; Max height
  (which-key-side-window-max-width 0.33)   ;; Max width
  (which-key-idle-delay 0.5)               ;; Delay before showing
  (which-key-max-description-length 25)    ;; Max description length
  (which-key-allow-imprecise-window-fit nil) ;; Precise window fit
  (which-key-separator " → ")              ;; Key separator
  (which-key-prefix-prefix "+")            ;; Prefix indicator
  :config
  ;; Custom which-key faces for better appearance
  ;; Use hook to set faces after which-key is fully loaded
  (add-hook 'which-key-mode-hook
            (lambda ()
              (when (facep 'which-key-key-face)
                (set-face-attribute 'which-key-key-face nil :foreground "#51afef" :weight 'bold))
              (when (facep 'which-key-command-description-face)
                (set-face-attribute 'which-key-command-description-face nil :foreground "#98be65"))
              (when (facep 'which-key-group-description-face)
                (set-face-attribute 'which-key-group-description-face nil :foreground "#c678dd" :weight 'bold))
              (when (facep 'which-key-separator-face)
                (set-face-attribute 'which-key-separator-face nil :foreground "#5b6268")))))

;; === VOLATILE HIGHLIGHTS - HIGHLIGHT CHANGES ===
;; Volatile highlights briefly highlights text changes
(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode t)
  :custom-face
  (vhl/default-face ((t (:background "#3f444a" :foreground "#ffffff")))))

;; === MINIMAP - CODE OVERVIEW ===
;; Minimap provides a miniature overview of your code
(use-package minimap
  :ensure t
  :custom
  (minimap-window-location 'right)         ;; Minimap location
  (minimap-width-fraction 0.1)             ;; Minimap width
  (minimap-minimum-width 20)               ;; Minimum width
  (minimap-automatically-delete-window t)  ;; Auto-delete when not needed
  :bind
  ("C-c m" . minimap-mode))               ;; Toggle minimap

;; === VISUAL REGEXP - BEAUTIFUL REGEX HIGHLIGHTING ===
;; Visual regexp provides real-time regex highlighting
(use-package visual-regexp
  :ensure t
  :bind
  ("C-c r" . vr/replace)                   ;; Visual replace
  ("C-c q" . vr/query-replace))            ;; Visual query replace

;; === GOGGLES - OPERATION HIGHLIGHTING ===
;; Goggles highlights regions affected by operations
(use-package goggles
  :ensure t
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t))

;; ============================================================================
;; 🎨 CUSTOM VISUAL TWEAKS - FINE-TUNING
;; ============================================================================

;; === WINDOW DIVIDERS - BEAUTIFUL BORDERS ===
;; Enhanced window dividers for better visual separation
(when (display-graphic-p)
  (window-divider-mode 1)
  (setq window-divider-default-bottom-width 1
        window-divider-default-right-width 1
        window-divider-default-places t)
  ;; Custom colors for window dividers
  (set-face-foreground 'window-divider "#5b6268")
  (set-face-foreground 'window-divider-first-pixel "#5b6268")
  (set-face-foreground 'window-divider-last-pixel "#5b6268"))

;; === FRINGE CUSTOMIZATION - SUBTLE IMPROVEMENTS ===
;; Customize fringe for better appearance
(when (display-graphic-p)
  (fringe-mode 10)  ;; Set fringe width
  (set-face-background 'fringe nil)  ;; Transparent fringe
  (set-face-foreground 'fringe "#5b6268"))  ;; Subtle fringe color

;; === CURSOR CUSTOMIZATION - ENHANCED VISIBILITY ===
;; Beautiful cursor configuration
(setq-default cursor-type 'box)           ;; Cursor type: box, bar, hbar
(setq blink-cursor-mode t)                ;; Enable cursor blinking
(setq blink-cursor-interval 0.6)          ;; Blink interval
(setq blink-cursor-delay 0.1)             ;; Blink delay

;; === LINE HIGHLIGHTING - SUBTLE EMPHASIS ===
;; Enhanced line highlighting
(global-hl-line-mode 1)
(set-face-background 'hl-line "#2c323c")  ;; Subtle highlight color

;; === SELECTION HIGHLIGHTING - BEAUTIFUL REGIONS ===
;; Enhanced region selection highlighting
(set-face-background 'region "#404854")   ;; Selection background
(set-face-foreground 'region nil)         ;; Keep original text color

;; ============================================================================
;; 🎯 PROVIDE & FINALIZATION
;; ============================================================================

;; Provide the UI module
(provide 'ui)

;; Display success message when module is loaded
(add-hook 'after-init-hook
          (lambda ()
            (when (featurep 'ui)
              (message "🎨 Theme System: Beautiful Catppuccin Mocha Active")
              (message "📊 Enhanced Modeline: Doom Modeline with Icons")
              (message "🏠 Professional Dashboard: Modern Startup Interface")
              (message "🔤 Typography: FiraCode Nerd Font with Ligatures")
              (message "🌈 Rainbow Delimiters: Colorful Code Structure")
              (message "✨ Beacon Mode: Cursor Location Highlighting")
              (message "🎯 Solaire Mode: Buffer Distinction Active")
              (message "📱 Centaur Tabs: Beautiful Tab Interface")
              (message "🔍 Dimmer Mode: Focus Enhancement")
              (message "⌨️ Which-Key: Beautiful Key Hints")
              (message "🎪 Visual Effects: Premium Experience Ready"))))

;;; ui.el ends here
