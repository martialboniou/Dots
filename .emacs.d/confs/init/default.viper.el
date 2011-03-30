; Viper Mode setup
(setq-default viper-mode t
              viper-inhibit-startup-message 't
              viper-ex-style-editing nil
              viper-expert-level '5
              viper-syntax-preference 'extended
              viper-always t)

; eshell is not a friend of Viper
(add-hook 'eshell-mode-hook
          (lambda ()
            (when viper-mode
              (setq viper-auto-indent nil))))
