(cond ((not (featurep 'booting)) (require 'kernel)))

(unless (locate-library "cl-lib") (error "preamble: cl-lib is required now!")) ; normally installed (using EL-SELECT via PACKS required by KERNEL)

(provide 'preamble)
