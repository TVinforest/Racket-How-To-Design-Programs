;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname text_editor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/universe) 
(require 2htdp/image)
(require 2htdp/batch-io)



;;STRINGS

;;prefix suffix
(define prefix "hello")
(define suffix "world")

(define string-test "helloworld")
(define i 5)

;;insert string at i position of another string
(define (insert-string str stri i )
  (string-append (substring str 0 i) stri (substring str i)))

;;remove character fomr i position of a string
(define (remove-1string str i )
  (cond
    [(equal? str "") ""]
    [else (string-append (substring str 0 (- i 1)) (substring str i))]))


;;get first 1string from non empty string 
(define (first-1string str)
  (cond
    [(equal? str "") ""]
    [else (substring str 0 1)]))


;;get last 1string from non empty string 
(define (last-1string str)
  (cond
    [(equal? str "") ""]
    [else (substring str (- (string-length str) 1))]))





;;background measurements
(define WIDTH (* 200 CHAR-WIDTH))
(define HEIGHT 40)

;;backgroud to draw on
(define BACKGROUND
    (rectangle WIDTH HEIGHT "solid" "black"))


;;cursor
(define-struct cursor (image position))

;;cursor instance  
(define cursor_mark
  (make-cursor
    (rectangle 1 20 "solid" "white") 120))
  
;;editor struct
(define-struct editor (text cursor))

;;editor instance
(define text_editor
  (make-editor (text "editor" 16 "red") cursor_mark))

;;render
(define (render editor)
 (place-image (cursor-image (editor-cursor editor)) (cursor-position (editor-cursor editor))  20
    (overlay/align "left" "center"
                   (editor-text editor)
                   BACKGROUND)))



;;update text on key
(define (update-text editor a-key)
  (cond
    ((key=? a-key "left")  (update-cursor-position a-key editor))
    ((key=? a-key "right") (update-cursor-position a-key editor))
    ((= (string-length a-key) 1) update-text)
    (else editor)))

;;select - or + operand based on key pressed
(define (update-cursor-position a-key editor)
  (make-editor (editor-text editor)
               (make-cursor (cursor-image (editor-cursor editor))
                             ((cond
                               ((key=? a-key "left") -)
                               ((key=? a-key "right") +)) (cursor-position (editor-cursor editor)) 12))))
    
;;main editor loop
(define (main editor)
  (big-bang editor
    (on-key update-text)
    (to-draw render)))
           













;;main editor loop
(define (texter ed)
  (big-bang ed
    (on-key edit)
    (to-draw render_ed)))


;;fontsize
(define FONT-SIZE 16)

;;1char width
(define CHAR-WIDTH (image-width (text " " FONT-SIZE "red")))


(define TEST (text "redtext" 16 "red"))



;;editor
(define-struct ed (pre post))

;;inst ed
(define ed_0
  (make-ed "hello" "red                                r"))
  
;;mark
(define CURSOR (rectangle 1 20 "solid" "white"))

;;render
(define (render_ed ed)
  (overlay/align "left" "center"
                 (beside (text (ed-pre ed) FONT-SIZE "red") 
                         CURSOR
                         (text (ed-post ed) FONT-SIZE "red"))
                         BACKGROUND))

;;edit
(define (edit ed ke)
 (cond
  ((and (equal? ke "\b") (not(left-edge? ed)))  (delete-char ed))
  ((and (allowed-glyph? ke) (can-add? ed)) (add-char ed ke) )
  ((and (equal? ke "left") (not(left-edge? ed)))  (move-cursor ed ke))
  ((and (equal? ke "right") (not (right-edge? ed))) (move-cursor ed ke)) 
    (else ed)))


;;pass characters right left to cursor 
(define (pass-char-right ed) 
  (make-ed (remove-1string (ed-pre ed) (string-length (ed-pre ed)))
           (string-append (last-1string (ed-pre ed)) (ed-post ed))))

(define (pass-char-left ed)
  (make-ed (string-append (ed-pre ed) (first-1string (ed-post ed)))
           (remove-1string (ed-post ed) 1)))

(define (move-cursor ed direction)
  (cond
    ((equal? direction "right") (pass-char-left ed))
    ((equal? direction "left") (pass-char-right ed))
    (else ed)))


;;adding character and deleting spaces in the tail if they collide with the boundaries of the text area
(define (add-char ed ke)
  (cond
    ((and (text-area-is-full ed) 
          (space-char-last? ed)
          (not(right-edge? ed)))  (make-ed (string-append (ed-pre ed) ke) (remove-1string (ed-post ed) (string-length (ed-post ed))))) ;add char and delete space char close to text area end
    (else                         (make-ed (string-append (ed-pre ed) ke) (ed-post ed))))) ;add char

;;define    
(define (remove-spaces-on-edge ed)       
  ( ... ))
;;delete character
(define (delete-char ed)
  (make-ed (remove-1string (ed-pre ed) (string-length (ed-pre ed))) (ed-post ed)))




;;current text image size
(define (current-width ed)
  (image-width  (beside (text (ed-pre ed) FONT-SIZE "red") 
                         CURSOR
                         (text (ed-post ed) FONT-SIZE "red"))))
                   
                 



;;allowed type of characters
(define (allowed-glyph? ke)
  (cond
    ((and (or (string-alphabetic? ke) (string-numeric? ke) (string-whitespace? ke))
          (equal? (string-length ke) 1)) #true)
    (else #false)))





;;<- can move cursor to right if !(& CursorOnTheRghtEdge TextAreaIsFull)
(define (can-move-right? ed)
  (not (and (not (equal? (image-width ed-post) 0))
            (text-area-is-full ed))))   

;;<- can move cursor to left if not on left edge

;;can delete a char if not on the left egde


;;can add char if !(& TextAreaIsFull ( | CursorOnTheRghtEdge  LastCharIsNotSpace)
(define (can-add? ed)
(not (and (text-area-is-full ed)                       ;text area is full 
          (or (right-edge? ed)                         ;cursor is on the right edge of text 
              (not (space-char-last? ed))))))                  ;last character is not a space



;;last character is a space
(define (space-char-last? ed)
 (string-whitespace? (last-1string (ed-post ed))))

;;TextAreaIsFull?
(define (text-area-is-full ed)
  (< (- (image-width BACKGROUND) (current-width ed)) (* CHAR-WIDTH 3)))

;;cursor is on the left edge of text
(define (left-edge? ed)
  (equal? (string-length (ed-pre ed)) 0))

;;cursor is on the right edge of text
(define (right-edge? ed)
  (equal? (string-length (ed-post ed)) 0))
