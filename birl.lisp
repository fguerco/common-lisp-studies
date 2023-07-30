#|
BIRL Language
https://birl-language.github.io/

O BIRL (Bambam's "It's show time" Recursive Language) é a linguagem de
programação mais treze já inventada.
Deve ser utilizada apenas por quem realmente constrói fibra e não é água com
código. É uma linguagem extremamente simples porém com poder para derrubar
todas as árvores do parque Ibirapuera.
Programando em BIRL, é verão o ano todo! 
|#

(defpackage #:birl
  (:use #:cl))

(in-package #:birl)

(defmacro hora-do-show (&body builder)
  `(progn ,@builder))

(defun ce-quer-ver-essa-porra? (&rest essa-porra)
  (format t "~{~a~^ ~}~%" essa-porra))

(defmacro que-que-ce-quer-monstrao? (var &body builder)
  `(progn
     (princ "que que ce quer monstrao? ")
     (let ((,var (read-line)))
       ,@builder)))

(defmacro ta-comigo-porra (&body builder)
  `(cond
     ,@(mapcar #'macroexpand-1 builder)))

(defmacro ele-que-a-gente-quer? (test &body builder)
  (list test `(progn ,@builder)))

(defmacro que-nao-vai-dar-o-que? (test &body builder)
  (list test `(progn ,@builder)))

(defmacro nao-vai-dar-nao (&body builder)
  (list t `(progn ,@builder)))

(defmacro oh-o-homi-ai-po (name lambda-list &body builder)
  `(defun ,name ,lambda-list
     ,@builder))

(defmacro ajuda-o-maluco-ta-doente (func &rest args)
  `(funcall #',func ,@args))


(defmacro mais-quero-mais (varlist endlist &body builder)
  `(do ,varlist ,endlist
     ,@builder
    monstro))

(defmacro negativa-bambam ((var value) loop-condition  &body builder)
  `(mais-quero-mais ((,var ,value)) ((not ,loop-condition))
    ,@builder))
        

(defmacro sai-filho-da-puta ()
  `(return))

(defmacro vamo-monstro ()
  `(go monstro))


;;;;
;;;; Exemplos
;;;;


(hora-do-show
  (mais-quero-mais ((m 0 (1+ m))) ((= m 10))
    (ta-comigo-porra
      (ele-que-a-gente-quer? (= m 7)
        (sai-filho-da-puta))
      (que-nao-vai-dar-o-que? (evenp m)
        (vamo-monstro)))

    (ce-quer-ver-essa-porra? "essa porra: " m)))

(negativa-bambam (x 5) (> x 2)
  (ce-quer-ver-essa-porra? "bora!" x)
  (decf x))

(hora-do-show
  (que-que-ce-quer-monstrao? x
    (ce-quer-ver-essa-porra? "bambam disse:" x)))
  
  
(ta-comigo-porra
  (ele-que-a-gente-quer? (> 1 2)
    (ce-quer-ver-essa-porra? "treze memo carai"))
  (que-nao-vai-dar-o-que? (< 11 2)
    (ce-quer-ver-essa-porra? "quero mais!"))
  (nao-vai-dar-nao
    (ce-quer-ver-essa-porra? "nao va dar nao")))

(oh-o-homi-ai-po bora! (vai)
  (ce-quer-ver-essa-porra? vai))

(ajuda-o-maluco-ta-doente bora! "É 37 anos carai!")
