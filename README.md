# CKern 82 - Kern 82 컴파일러

### ⚠️⚠️ 급구!! 하스켈 능력자님 도와주세요!!

<small>제가 하스켈을 할 줄 몰라서 GPT-6 아스트라로 때웠습니다.. 하스켈 2000줄 가량이 부분적으로 블랙 박스인 상황입니다. 도와주세요..</small>

## ❓ Kern 82가 무엇인가요?  
Kern 82는 라이히스팍트 세계관에서 1980년대 독일 제국에서 국방/민간 복수 표준 용도로 개발된 언어입니다.

독일 제국은 1960년대와 1970년대 C, BASIC, COBOL과 같은 언어들이 들어오면서 냉전 초기의 정보 과학 산업에서의 우세를 잃어가고 있었습니다. 

1977년 카이저 빌헬름 연구소에서는 이 언어들을 대체하고 민간/국방 분야에서 복수 표준으로 프로그래밍 언어를 개발하기 시작했고, C언어와 Pascal을 기반으로 개발을 시작했습니다.

한편, 1979년, 덴마크에서 C with Classes가 개발되었고, 같은 해 독일 제국에서 미국 Ada의 전신이 되는 남프랑스에서 제작한 Green 또한 손에 넣으면서 Kern에 객체지향형 프로그래밍의 요소가 들어가게 되었고 현재의 Kern 82가 1982년에 만들어졌습니다. 

이후 Kern 82는 민간/군사 복수 표준이 되었지만, 언어 자체로는 모듈이 부족했고, 많은 비표준 모델들이 난립하다가 1990년 Kern 90으로 통합되었습니다.

위 프로젝트는 당시에는 없었지만, C99와 Haskell을 사용해서 당시의 Kern 82 컴파일러를 구현한 CKern 82입니다.

## 🌟 Kern 82의 특징

* 강한 정적 타입
* 타입 종류 `GANZ`, `LANG`, `GLEIT`, `BOOL`, `CHAR`
* 고정 용량 문자열 `KETTE`
* 고정 크기 다차원 배열
* `TYP`, `ENUM`, `STRUCT`
* `PUBLIC` 과 `PRIVATE`
* 함수 `FUNKTION` 과 프로시저 `PROZEDUR`
* 포인터와 참조
* 동적 메모리 할당
* 런타임 lifetime / bounds 검사
* 함수 포인터
* 모듈 시스템
* C99 백엔드

## ⚙️ 다운로드/컴파일 방법

#### GCC C Compiler를 선행해서 설치해야 CKern 82를 사용할 수 있습니다. 

만약 GCC 컴파일러가 설치되어 있다면, CKern 82를 다운로드 한 경로를 Windows 환경변수에다가 넣어주세요. ( GCC C Compiler에서 했던 작업과 비슷하게 하면 됩니다. )

GCC를 처음 설치한다면
[한국어 GCC 설치 안내](docs/GCC_INSTALL.md)를 참고하시면 됩니다.

설정이 다 되면 다음 명령어를 사용 할 수 있게 됩니다.

```powershell
kern run "C:\내 프로젝트\hello.k82"
kern "C:\내 프로젝트\hello.k82"            # run과 동일
kern build "C:\내 프로젝트\hello.k82"      # 소스 옆 hello.exe 생성
kern check "C:\내 프로젝트\hello.k82"      # GCC 없이 검사만
kern --help
kern --version
```

저장소에서 PATH 설정 없이 쓰려면 `kern` 대신 `.\bin\kern.exe`를 사용하시면 됩니다.


 ## 📚 예제들

--- 
### Hallo, Welt!
---

```
PROZEDUR main()
BEGIN
    SCHREIBEN("Hallo, Welt!");
END
```

--- 
### 별 정삼각형 출력
---
```
PROZEDUR main()
BEGIN
    GANZ x = 0;
    LESEN(x);
    FOR (GANZ i = 0; i < x; i++)
    BEGIN
        FOR (GANZ j = 0; j < 2 * x -1; j++)
        BEGIN
            IF j >= x-1-i AND j <= x-1+i THEN
            BEGIN
                SCHREIBEN("*");
            END
            ELSE
            BEGIN
                SCHREIBEN(" ");
            END
        END
        AUSGABE("");
    END
END
```
--- 
### 재귀함수 팩토리얼
---
```
FUNKTION GANZ factorial(GANZ x)
BEGIN
    IF x == 1 THEN
    BEGIN
        RÜCKGABE 1;
    END
    RÜCKGABE x*factorial(x-1);

END

PROZEDUR main()
BEGIN
    GANZ x = 0;
    LESEN(x);
    GANZ y = factorial(x);
    AUSGABE(y);  
END
```

더 많은 예제들은 [`/examples`](./examples)에서 찾아보실수 있습니다.

## ✒️ 문법

> 나는 Kern 82의 문법을 적을 완벽한 방법을 발명했지만 여백이 부족함으로 [`/docs`](./docs)에 분리해서 적었다.

문법 내용이 방대함으로 따로 문서로 정리하였고, 아직 채워넣고 있는 문서이기 때문에 미완성일 수 있습니다.
* [`/docs/01_입출력.md`](./docs/01_입출력.md) 입출력
* [`/docs/02_변수와 자료형.md`](./docs/02_변수와자료형.md) 변수와 자료형
* [`/docs/03_연산자.md`](./docs/03_연산자.md) 연산자
* [`/docs/04_제어문.md`](./docs/04_제어문.md) 제어문

-- 현재 여기까지 완성되었습니다. --
* [`/docs/05_함수와 프로시저.md`](./docs/05_함수와프로시저.md) 함수와 프로시저
* [`/docs/06_배열과 KETTE.md`](./docs/06_배열과KETTE.md) 배열과 문자열
* [`/docs/07_TYP와 ENUM.md`](./docs/07_TYP와ENUM.md) TYP와 열거형
* [`/docs/08_STRUCT.md`](./docs/08_STRUCT.md) 구조체
* [`/docs/09_포인터와 REFERENZ.md`](./docs/09_포인터와REFERENZ.md) 포인터와 간접참조
* [`/docs/10_동적 할당.md`](./docs/10_동적할당.md) 동적할당
* [`/docs/11_제네릭.md`](./docs/11_제네릭.md) 제네릭
* [`/docs/12_모듈과 표준 라이브러리.md`](./docs/12_모듈과표준라이브러리.md) 모듈과 표준 라이브러리


## 💾 표준 라이브러리

표준 라이브러리 항목은 다음과 같습니다.

* [`/stdlib/Mathematik.k82`](/stdlib/Mathematik.k82) 수학

모든 표준 라이브러리 항목은 가능한 Kern으로 작성되며, 표준 라이브러리 소스는 [`/stdlib`](./stdlib)에서 확인 할 수 있습니다.

## 🛠️ 컴파일러 구조

현재 컴파일러의 구조는 Haskell로 구현되고 있으며 C99 코드를 생성합니다. 추후에 x86-64 Assembly로 변경할 계획이 있습니다.

## 🕛 현재 개발 상황
Kern 82는 현재 베타 테스트를 진행하고 있으며, 추가적인 표준 라이브러리나 문법이 제공될 가능성이 있습니다.

또한 버그도 있을 수 있으니, 버그 제보는 언제든지 디스코드를 통해 해주시기 바랍니다.

## 🗨️ 크레딧

본 프로젝트는 라이히스팍트 세계관의 미독 냉전에 대해서 구상하던 중에 갑자기
> 독일 제국에도 번듯한 프로그래밍 언어 하나 있어야지!!

해서 시작하게 되었습니다. 프로그래밍 언어 구성은 적당히 알고 있었지만.. 제가 하스켈을 모른다는 사소한 찐빠가 있었는데..

GPT-6 Astra가 참 코딩을 잘하더라고요. 9월 15일에 시작해서 컴파일러 개발은 17일에 마무리 지었습니다. 예.. 3일 걸렸습니다. 

그럼 나머지 시간은 무슨 일 했냐고요? 예.. `README.md`랑 `\doc` 에 있는 마크다운 파일 작성하느라 다 보냈습니다. 

아무튼 가상 언어에서 이제 현실 언어가 된 Kern 82 많이 써주셨으면 감사하겠습니다. 또 지금 컴파일러 구조가 하스켈을 모르는 제가 봐도 뭔가 개판이거든요?? 그래서 컴파일러 리팩토링 도와주실 분을 모집하고 있습니다. 하스켈로 컴파일러 만드실 줄 아는 분 있으시면 디스코드로 연락주세요..