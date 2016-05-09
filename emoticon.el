(require 'cl-lib)
(require 'helm)
(require 'emoticon-data)
(defvar emoticon-candidates-limit 9999)
(defvar emoticon-buffer-name "*Emoticon*")
(defvar emoticon-prompt "Search Emoticon: ")

(defun emoticon ()
  (interactive)
  (helm :sources (helm-build-sync-source "Please input pattern to search Emoticon: "
                   :candidates (lambda () (emoticon-get-candidates helm-pattern))
                   :volatile t
                   :action (lambda (str) (insert (emoticon-process-the-string-to-insert helm-pattern str)))
                   :candidate-number-limit emoticon-candidates-limit)
        :buffer emoticon-buffer-name
        :prompt emoticon-prompt))

(defun emoticon-get-candidates (pattern)
  "Get candidates list from `emoticon-internal-get-candidates',
align & format the them as ((DISPLAY . REAL-EMOTICON) ...)"
  (let* ((candidates-list (emoticon-internal-get-candidates pattern))
         (max-length (emoticon-max (emoticon-collect-lengths-of-matched-string candidates-list))))
    (mapcar (lambda (x)
              (cons (concat (propertize (car x) 'face 'font-lock-keyword)
                            (make-string (- max-length (string-width (car x))) 32) ;`32' is space
                            " => "
                            (propertize (cdr x) 'face 'bold)) ;DISPLAY
                    (cdr x)))     ;REAL
            candidates-list)))

(defun emoticon-internal-get-candidates (pattern)
  "Return ((MATCHED-STRING . EMOTICON) ...)"
  (cl-remove-if
   #'null
   (mapcar (lambda (row)
             (let ((matched (cl-member pattern (car row)
                                       :test (lambda (pat str) (string-match pat str)))))
               (if matched
                   (cons (car matched) (cdr row)) ;Return this
                 nil)))          ; this will be removed
           emoticon-table
           )))


(defun kaomoji-collect-lengths-of-matched-string (list)
  (mapcar (lambda (x) (string-width (car x)))
          list))

(defun emoticon-max (list)
  (if (and (listp list) list)
      (apply #'max list)))

(defun emoticon-matched-pattern? (user-input)
  "See variable `emoticon-patterns-inserted-along-with'"
  (cl-some (lambda (pattern)
             (string-match pattern user-input))
           emoticon-patterns-inserted-along-with))

(defun emoticon-process-the-string-to-insert (user-input emoticon-string)
  "Check if should concatenate USER-INPUT to EMOTICON, then return
the string for `insert'"
  (if (not (emoticon-matched-pattern? user-input))
      emoticon-string
    (if (eq 'left-side emoticon-insert-user-input-at)
        (concat user-input " " emoticon-string)
      (concat emoticon-string " " user-input))))

(provide 'emoticon)


(defvar emoticon-table
  '((("angry" "table" "生氣" "翻桌" "幹")                           . "(╯°□°）╯︵ ┻━┻" )
    (("hehe" "真是個壞女孩" )                                       . "∩ __∩ y" )
    (("cry" "泣き")                                                 . "・ﾟ・｡･ﾟ･(つД｀)" )
    (("cry" "泣き" "哭哭")                                          . "(´;ω;`)" )
    (("cry" "泣き" "哭哭")                                          . "(´；д；｀)" )
    (("cry miserably" )                                             . "(;´༎ຶД༎ຶ`)" )
    (("dandin" "淡定" "淡定紅茶")                                   . "ˊ_>ˋ" )
    (("heart" "ハート" "愛心")                                      . "♥" )
    (("homo" "ホモ")                                                . "┌（┌ ＾o＾）┐ﾎﾓｫ" )
    (("kita" "来たー" "キター" "きたー")                            . "ｷﾀ――(ﾟ∀ﾟ)――!!" )
    (("lazy" "懶")                                                  . "_(:3 」∠ )_ " )
    (("owo" )                                                       . "ˊ・ω・ˋ" )
    (("relax" )                                                     . "ˊvˋ" )
    (("uwu" )                                                       . "( ˘ω˘ )" )
    (("sad")                                                        . "(´･_･`)")
    (("Huh?" "不知所云" "摳妞臉")                                   . "(ﾟ⊿ﾟ)")
    (("WTF" "什麼啦")                                               . "(｡ŏ_ŏ)")
    (("owo")                                                        . "( ˘•ω•˘ )")
    (("relax")                                                      . "(´-ω-｀) ")
    (("happy" "爽翻")                                               . "(｡A｡) ")
    (("yeah" ">w<")                                                 . "(ﾉ>ω<)ﾉ ")
    (("rock")                                                       . "\\m/ >_< \\m/")
    (("cheers" "歡呼")                                              . "｡:.ﾟヽ(*´∀`)ﾉﾟ.:｡ ")
    (("cheers" "歡呼")                                              . " ヾ(*´∀ ˋ*)ﾉ ")
    (("owo")                                                        . "(´・ω・`)")
    (("happy" "爽爽")                                               . "(ﾟ∀。)")
    (("don't see" "非禮勿視")                                       . "(つд⊂) ")
    (("alas" "無奈" "面對國民黨支持者的表情" "面對中國五毛的表情")  . "╮(╯_╰)╭ ")
    (("haha" "你看看你" "UCCU" "uccu")                              . "σ ﾟ∀ ﾟ) ﾟ∀ﾟ)σ")
    (("me?" "我嗎?")                                                . "σ(´∀｀*)")
    (("don't mind")                                                 . "il||li _|￣|○ヽ(･ω･｀) ")
    (("butt" "お尻" "おしり" "摳屁股")                              . "_(:3 ⌒ﾞ)_ ")
    (("five cent hello" "五毛你好")                                 . "(・∀・)つ➄ ")
    (("yeah" "耶")                                                  . "ε≡ﾍ( ´∀`)ﾉ ")
    (("i would be angry" "我要生氣了")                              . "(・`ω´・)")
    (("really?" "orly" "呵呵" "是嗎" "是喔")                        . "(≖ᴗ≖๑)")
    (("shocked" "震驚" "驚訝")                                      . "(ﾟдﾟ)")
    (("so pity" "你好可憐喔")                                       . "。･ﾟ･(つд`ﾟ)つ⑩))Д´)")
    (("come here" "來來來")                                         . "ლ(´ڡ`ლ)")
    (("angry" "furious")                                            . "(／‵Д′)／~ ╧╧ ")
    (("angry" "punch")                                              . "#ﾟÅﾟ）⊂彡☆))ﾟДﾟ)･∵")
    ))
