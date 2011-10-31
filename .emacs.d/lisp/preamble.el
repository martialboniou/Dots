(add-to-list 'load-path (expand-file-name (file-name-directory load-file-name)))

(cond ((not (featurep 'booting))     (require 'kernel)))
(cond ((not (featurep 'programming)) (require 'code)))

(provide 'preamble)
