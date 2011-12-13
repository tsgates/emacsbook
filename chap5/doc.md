%
% Taesoo Kim
%
% title: Emacs 시작하기
%
% abstract: 
% abstract: 
%

XXX.

= 키맵 (keymap)

사용자가 키를 입력하면 이맥스는 키입력에 바인드된 함수를 찾아 호출 한다. 키입력과
바인드된 해당 함수의 정보를 담고있는 변수를 맵(map)이라고 부른다. 맵은 아래와
같이 키와 값을 짝으로 갖는 Associate List(ALIST)의 구조로 표현된다.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.cl}
;; global-map
(list ("C-a" . 'move-beginning-of-line)
      ("C-e" . 'move-end-of-line)
      ...)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

즉 맵의 한원소는 \f{car}의 키값 \f{cdr}의 해당 함수의 심볼로 이루어진다. 또한
여러개의 맵이 체인으로 아래와 같이 엮어 있고, 키를 입력하면 왼쪽으로 부터
순차적으로 해당 함수를 찾게 된다.

    +-------------+    +-----------------+    +-----------------+    +------------+
    | local-map?  |--->| minor-mode-map* |--->| major-mode-map+ |--->| global-map |
    +-------------+    +-----------------+    +-----------------+    +------------+

왼쪽의 3개의 맵들, \v{local-map}, \v{minor-mode-map}, \v{major-mode-map}은 해당
버퍼 안에서의 키입력에 영향을 미치는 맵이고, \v{global-map}은 global한 이맥스의
키입력에 영향을 미치는 맵이다. 

이맥스에서 우리의 키입력이 현재 버퍼에서 어떤 맵에 의해서 어떠한 함수로
해석되는지 알아 볼 수 있는데, \k{C-h b:바인딩 도움말} (**h**elp **b**inding)을
실행시켜보자. 입력한 키에 대해서 위에서 부터 차례로 해당하는 함수를 찾게되며 그
중 우리가 특별하게 관심있는 \t{global-map}을 찾아 보면, 아래와 같은 화면을 볼 수
있을 것이다.

![\n{img} \v{global map}](\s{snap -o emacs-help-binding.png
  -c C-h b C-x 0 C-s global})

XXX.

= 모드 (mode)

- local-map?
- minor-mode-map*
- major-mode-map+

= 커서이동 (moving)

이맥스의 기본적인 에디팅 기능들(커서이동, 수정, 검색 ..)은 모든 버퍼에서
공통적으로 요구되는 기능이기 때문에 \v{global-map}에서 정의 되어 있다. 이번
장에서는 \v{global-map}에 정의된 기본적인 커서이동 방법에 대해서 알아 보도록
하자.

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