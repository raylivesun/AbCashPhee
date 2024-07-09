(provide 'servers)
(provide 'cloudservices)
(provide 'paths)
(provide 'services)
(provide 'arguments)
(provide 'workers)
(provide 'benefit)
(provide 'checkup)
(provide 'health)
(provide 'status)
(provide 'start)

;; connection of workers is not supported
;; works only with connections benefit
;; works only with connections health
;; works only with connections checkup
;; works only with connections darts
;; works only with connections status
;; works only with connections start

'(servers cloudservices
  (paths services)
  (arguments workers)
  (benefit checkup)
  (health status)
  (start benefit))

;; works only with connections start and benefit check up done
'(servers (it benefit) (check up) (documentation done))

;; works only with connections start and benefit check
(car '(it benefit check up and documentation done))

;; works only with connections start and benefit check
(cdr '(it benefit check up and documentation done))

;; works only with connections start and benefit check
(car '(it benefit check up and connection in matrix done))

;; works only with connections start and benefit check
(car '((it benefit check up and documentation in matrix done)
       (it benefit check up and connection in matrix done)
       (it benefit check up and servers in matrix done)
       (it benefit check up and cloudservices in matrix done)
       (it benefit check up and paths in matrix done)
       (it benefit check up and services in matrix done)
       (it benefit check up and arguments in matrix done)
       (it benefit check up and workers in matrix done)
       (it benefit check up and benefit in matrix done)
       (it benefit check up and checkup in matrix done)
       (it benefit check up and health in matrix done)
       (it benefit check up and status in matrix done)
       (it benefit check up and start in matrix done)))


;; works only with connections start and benefit check
(cdr '((key-validation)
       (key-documentation)
       (key-access)
       (key-connection)
       (key-servers)
       (key-cloudservices)
       (key-paths)
       (key-services)
       (key-arguments)
       (key-workers)
       (key-benefit)
       (key-checkup)
       (key-health)
       (key-status)
       (key-start)))

;; with the following line to get the services
(car '(((read-key-services))
       (write-key-services)))

;; with the following line to get the services
(cdr '(((read-key-services))
       (write-key-services)))
;; with the following line to get the services
(car '(((this-key-services-command))
       (this-key-services-command)))

;; with the following line to get the services
(cdr '(((this-key-services-command))
       (this-key-services-command)))


;; with the following line to get the services
(car '((define-key)))


;; with the following line to get the services
(cdr '((define-key)))

;; with the following line to get the services

(defun frame-relative-coordinates (position)
  "Return frame-relative coordinates from POSITION.
POSITION is assumed to lie in a window text area."
  (let* ((x-y (posn-x-y position))
         (window (posn-window position))
         (edges (window-inside-pixel-edges window)))
    (cons (+ (car x-y) (car edges))
          (+ (cdr x-y) (cadr edges)))))


