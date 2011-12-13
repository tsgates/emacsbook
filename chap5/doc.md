%
% Taesoo Kim
%
% title: Emacs 시작하기
%
% abstract: 
% abstract: 
%

이번 장에서는 ...;

사용자가 키를 입력하면 이맥스는 키입력에 바인드된 함수를 찾아 호출 한다. 키입력과
바인드된 해당 함수의 정보를 담고있는 변수를 맵(map)이라고 부른다. 맵은 아래와
같이 키와 값을 페어로 갖는 Associate List(ALIST)의 구조로 표현된다.

    global-map:
      (list ("C-a" . 'move-beginning-of-line)
            ("C-e" . 'move-end-of-line)
            ...)

즉 맵의 한원소는 \f{car}의 키값 \f{cdr}의 해당 함수의 심볼로 이루어진다.

+-------------+  +------------------+  +------------------+  +------------+
| local-map? --> | minor-mode-map* --> | major-mode-map+ --> | global-map |
+-------------+  +------------------+  +------------------+  ++-----------+

- moving

	C-f (**f**orward-char)
	C-b (**b**ackward-char)

	M-f (**f**orward-word)
	M-b (**b**ackword-word)

        C-M-f (**f**orward-sexp)
        C-M-b (**b**acword-sexp)
    
	C-n (**n**ext-line)
	C-p (**p**revious-line)

	C-a (beginning-of-line, **a**head of line)
	C-e (**e**nd-of-line)

	M-a (backward-sentence, **a**head of sentence)
	M-e (forward-sentence, **e**nd of sentence)

        M-{ (backward-paragraph)
        M-} (forward-paragraph)
	
        C-M-a (beginning-of-defun, **a**head of defun)
        C-M-e (end-of-defun, **e**nd of defun)

        C-M-d (down-list)
	C-M-u (backward-up-list)
	
        C-v (scroll-up)
        M-v (scroll-down)

        C-M-v (scroll-other-window-up)
        C-M-S-v (scroll-other-window-down)
	
- jumping

    C-u C-SPC 
    M-< (beginning-of-buffer)
    M-> (end-of-buffer)
    M-g g (goto-line)
    M-g n (next-error)
    M-g p (previous-error)
    C-M-v
    C-M-S-v
    
- search

    C-s isearch-forward
    C-r isearch-backward

    C-M-s isearch-forward-regexp
    C-M-r isearch-backward-regexp
    
- editing

    DEL delete-backward-char
    M-DEL backward-kill-word
    C-DEL delete-kill-word
    C-d delete-char
    M-d kill-word
    M-k kill-line
    C-M-k kill-sexp
    M-z zap-to-char
    C-t 
    M-t 
    C-M-t
    C-o
    C-x C-o
    C-j
    
- register

=> end with how to modify global-map and permanently change it