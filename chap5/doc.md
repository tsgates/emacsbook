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

             +---------- 사용자의 키 입력
             |
    +--------v--------+
    | local-map?      | (("C-a" . `soo-magic-beg-of-line) ...)
    +--------|--------+
    +--------v--------+
    | minor-mode-map* | (("C-." . `flyspell-auto-correct-word) ...)
    +--------|--------+
    +--------v--------+
    | major-mode-map+ | (("M-e" . `c-end-of-statement) ...)
    +--------|--------+
    +--------v--------+
    | global-map      | (("C-a" . `move-beginning-of-line) ...)
    +-----------------+

왼쪽의 3개의 맵들, \v{local-map}, \v{minor-mode-map}, \v{major-mode-map}은 해당
버퍼 안에서의 키입력에 영향을 미치는 맵이고, \v{global-map}은 global한 이맥스의
키입력에 영향을 미치는 맵이다. 

이맥스에서 우리의 키입력이 현재 버퍼에서 어떤 맵에 의해서 어떠한 함수로
해석되는지 알아 볼 수 있는데, \k{C-h b:바인딩 도움말} (**h**elp **b**inding)을
실행시켜보자. 입력한 키에 대해서 위에서 부터 차례로 해당하는 함수를 찾게되며 그
중 우리가 특별하게 관심있는 \t{global-map}을 찾아 보면, 아래와 같은 화면을 볼 수
있다.

![\n{img} \v{global map}](\s{snap -o emacs-help-binding.png
  -c C-h b C-x 0 C-s global})

\k{C-h k}의 입력된 키에 대한 함수를 찾는 것과는 달리, 지금 알아본 \k{C-h b}는
사용자가 현재 버퍼에 대하여 입력할 수 있는 모든 키 입력와 해당 하는 함수를
한눈에 볼 수 있어 유용하게 쓰인다.

= 모드 (mode)

일반적으로 우리가 특정 파일을 이맥스에서 열었을때 이맥스는 열린 파일에 대한
버퍼를 생성하하고, "버퍼"의 이름에서 알 수 있듯이 사용자는 파일을 직접 수정하는
것 대신 버퍼를 수정하게 된다. 이때 현재 작업하는 버퍼(파일)에 대하여 특별히
유용한 함수들와 키맵(map)의 그룹을 불러 들이게 된다. 예를 들면 C언어로 짜여진
파일을 열었을 경우, C언어에 특화된 함수와 변수(코드 하이라이팅, 주석 처리 등)를
불러들이고, 이러한 함수와 변수를 하나로 모아서 모드(mode)라고 부른다. 즉 C언어로
짜여진 파일을 열면 (".c"의 확장자를 갖는 파일을 열면), 현재 버퍼의 `major-mode`가
`c-mode`가 된다. 버퍼를 기능별로 분류하다 보니, 자연스럽게 하나의 버퍼당 하나의
`major-mode`를 갖게 되며, 현재 버퍼의 모드는 `major-mode`의 변수에 기록이
된다. \f{C-h v: 변수 도움말}을 통해 한번 확인해 보도록 하자.

버퍼에 하나씩 할당되는 기본적인 주 모드 이외에, 유틸리적 특성을 갖는 함수와
변수의 집합을 하나로 묶어 `minor-mode`라고 부른다. 예를 들면 스팰링 체크를 위한
`flyspell-mode`와 편리한 네비게이션을 위한 `ido-mode` 등이 있다. 이와 같은
`minor-mode`는 하나의 버퍼에 여러개의 모드를 적용할 수 있고, `major-mode`와
유사하게 `minor-mode-list`의 변수에 기록된다.

정리하면, 하나의 모드(mode)는 유사한 기능을 위한 함수/변수들과, 사용자가 어떠한
키입력으로 호출 하는지에 관한맵(map)으로 이루어져 있다. 하나의 버퍼는 주된 기능을
나타내는 `major-mode` 하나와 사용자에 필요에 따라 여러개의 `minor-mode`를 불러
들일 수 있고, 사용자가 키를 입력 하면 `minor-mode`들과 `major-mode`의 맵을
순차적으로 검색하고 해당 함수를 찾아 호출 한다.

`major-mode`와 `minor-mode`는 일반적으로 다른 프로그래머가 일반적인 목적으로
작성된 것으로, 사용자가 버퍼에 해당하는 키입력을 변경하고자 할때에는,
`local-map`을 변경하여 사용자의 기호에 따라 키바인딩(키와 해당 함수)을 변경하게
된다. 즉, 중복적으로 정의된 키바인딩은 맵을 찾는 순서에 따라, `local-mode`에
정의된 맵이 가장 우선순위를 갖게 되고, `minor-mode`와 `major-mode`가 차례로
검색이 된다.

= 커서이동 (moving)

이맥스의 기본적인 에디팅 기능들(커서이동, 수정, 검색 ..)은 모든 버퍼에 일관적으로
요구되는 기능이기 때문에 \v{global-map}에 정의 되어 있다. \k{C-h t:튜토리얼}에서
살펴본 대부분의 에디팅 기능들 역시 \v{global-map}에 정의되어 있으며, 대부분의
경우 특정 모드에 상관 없이 대부분 적용되는 기능들로 이맥스의 일관적인 철학을
엿볼 수 있기도 하다. 이번 절에서 간단한 커서 이동법을 살펴보도록 하자.

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