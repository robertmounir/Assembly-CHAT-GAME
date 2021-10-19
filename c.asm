include Mcrs.inc
.model small
.Stack 64
.data 
;---------------------------- main Menu Variables ---------------------------- 
GameMessage db "Play Icy Tower Game$"
ChatMessage db "Chat$"
Exitmes db "Exit$"
Choosen db 0 ;Incremented by 3
;0-> Game
;3-> Chat
;6-> Exit
choosenColor equ 7
MessX equ 20
MessY equ 8
iamfirst db 0
;-------------Serial port ---------------------------

InvitationSent db 0
InvitationRecieved db 0

GameInvitation equ 'G'
ChatInvitation equ 'C'

ValueS db 0 ; used in sending data
ValueR db 0 ; used in Recieved data
dummy equ 0ffh
;----------------Controller-----------------
Controller_Data label byte
db dummy ;Valid start
CharacterX DW SWidth/3 
CharacterY DW SHeight-StepHeight ;Buttom of screen
CharacterX2 DW 2*SWidth/3 
CharacterY2 DW SHeight-StepHeight ;Buttom of screen
CardC1 db 0 ; card taken by p1
CardC2 db 0 ; card taken by p2
TScore1 dw 0
TScore2 dw 0
Gamewinner db 0 
StartChat db 0
Flags db  0  ;for both players 0[Scroll]0000[p2 freezed][p1 freezed]

BytesNum equ 18

;----------------otherplayer-----------------

btn db 0   ; 'l' : left  'r': right  'j':jump  'e':endgame  
;-------------------------Chatting variables --------------------------------
	csx         db 0
	csy         db 0
	crx         db 0
	cry         db 0
	MyName  db 15d,?,16d dup('$')

	endmsg1     db '- To end chatting with ','$'
	OtherName db 15d,?,16d dup('$')
	endmsg2     db 'Press ESC','$'
	entermsg1   db 'Please enter your name :','$'
	entermsg2   db 'Press enter to continue... ','$'
    faultinMyName db 0
    value db '?'
    Level1ms db '1 -> level1$ '  
    Level2ms db '2 -> level2$'  
    level db 0

;-------------------------Game Variables----------------------------------------
;-------------------------Changing Video Mode Parameters For Game --------------
SWidth equ 640
SHeight equ 400

;---------------------Game Parameters
;---------------------MaxJumpDistance=(MaxVel)*JmpStrength ---- 210-=(30)*10-90

Gravity equ 25
MAXVEL equ cardlength-1
Xpar equ 10 ;X Acceleration value (when a button pressed) 
JmpStrength equ 4
Fric equ 7;X decacceleration
Delay equ 0a2C2h;Used in frame Drawing
StepHeight equ 5
StepsColor equ 9
ScoreFactor equ 10d
MinJumpVel equ Gravity/jmpStrength;min velocity that makes him can jump with 0 velocity   
minJmpStrength equ Gravity*2
Card4Bounus equ 50
ScrollHeight equ 150d
scrollspeed equ 5 ;amount to scrol every time
Towerlength equ 2000d
stepInBetween equ 60d  
;----------------------------------------LvL Design---------------------
;----------------------------------------Each Step has Length , X and Y
;----------------------------------------Can Draw As Many Steps as you want
;----------------------------------------StpN is Steps Number------ 

stpsN equ TowerLength/stepInBetween
Stps    DW SWidth , 0 ,SHeight-StepHeight ; Len , X , Y
        DW stpsn-1 dup(3 dup(?))
randseed dw 0abcdh  
;------------------------Character---------------------------
charData ;See Macro File
;-------------------------Character variables--------------------
XDir DB 0 ;0 stop 1 right -1 left
VelocityX DW 0
VelocityY DW 0
AccelerationX DW 0
IsLanded db 1 ; 0-> Jumping 1->Landed 
TimePan1 db 0
Score dw 0 ;Max Height reached / Time to reach that Height
Bounus dw 0
charactertoDraw db 0 ;0->usual character 1->el z3ama 
;-------------------------Character 2 variables--------------------
XDir2 DB 0 ;0 stop 1 right -1 left
VelocityX2 DW 0
VelocityY2 DW 0
AccelerationX2 DW 0
IsLanded2 db 1 ; 0-> Jumping 1->Landed 
TimePan2 db 0
Score2 dw 0 ;Max Height reached / Time to reach that Height
Bounus2 dw 0
charactertoDraw2 db 0 ;0->usual character 1->el z3ama 

;-------------------Game variables---------------------------
ChangeDir db -1;just used in multiplication

Start_Second dw ? ;(Hours%12*60+minutes)*60+seconds
GameOver db "Game Over$"
Winneris db"GameWinner is : $"
Pressany db "Press Any Key to Continue .....$" 

;----------------Cards------------------------
;----offset-color-IsUsed-CardNumber 
;1->Go down to first step 
;2->Go up 1 Step
;3->freeze
;4->increase score 
;5->freeze other player
;6->change character 

Cards dw 320, 0, 0
  
        DW stpsn-1 dup(3 dup(0))

Cardlength equ 26 
CardsNum dw stpsn-7
cardTypes equ 7
X dw ?
Y dw ?
W db ?
H db ?
imgofs dw ?

.code
main proc far
mov ax,@data
mov ds,ax
    
        call ConnectionInit
        call usernamescreen
        call ExchangeNames
MainMenu:
        mov invitationrecieved,0
        mov InvitationSent,0

        call DrawMainMenu
        call GetUserChoice
        cmp choosen ,0
        jne NotGame 
;----------------Check if i have invitation------------------
        mov dx , 3FDH ; Line Status Register
        in al , dx 
        test al , 1
        Jz SENDGINV ;Didn't Recieve invitation
        ;If Ready read the VALUE in Receive data register
        mov dx , 03F8H
        in al , dx
        mov INVITATIONRECIEVED , al
        
        cmp INVITATIONRECIEVED,GameInvitation
        jne SendGinv

        mov valueS,GameInvitation
        call SendByte ;accept invitation

    wrong:
        call RecieveByte
        cmp valueR,dummy
        jne wrong
    
        call RecieveByte
        mov al,valuer        
        mov byte ptr start_second,al
        
        call RecieveByte
        mov ah,valuer
        mov byte ptr start_second+1,ah

        call RecieveByte
        mov al,valuer
        mov level,al
       
        call Game
        jmp MAINMENU
;------------------Send invitation  -------------------------------------
    SendGinv:
        mov valueS,Gameinvitation
        call SendByte
        call RecieveByte

        cmp valueR,Gameinvitation
        jne MAINMENU ;didn't accept
    
        mov InvitationSent,Gameinvitation
    
        call GameWithLogic
        jmp MAINMENU

NOTGAME:
        cmp choosen,3 ;Chat 
        je ChatStart

        jmp EndProgram
chatStart: 
        call Chat
        jmp MAINMENU
EndProgram:
        mov dh,MessY+12
        mov dl,MessX-2
        mov ah ,02    
        int 10h
        MOV AH, 4CH                  ; return control to DOS
        INT 21H
        ret
main endp 
ChooseLevel proc near
        mov  ah,0
        mov  al,3h
        int  10h
        
        mov dh,MessY
        mov dl,MessX
        mov ah ,02    
        int 10h
        showMessage level1ms
        mov dl,MessX
        mov ah ,02    
        mov dh,MessY+3
        int 10h
        showMessage level2ms
       validate: 
        mov ah,0
        int 16h
        cmp al,31h
            jne notlvl1
              mov level,1
              jmp endlvl
        notlvl1:
        cmp al,32h
        jne validate
        mov level,2
        endlvl:
        ret
ChooseLevel endp
ExchangeNames proc near
        mov dx , 3FDH ; Line Status Register
        in al , dx 
        test al , 1
        JZ second ;Not Ready

        WaitJoin:
        mov values,dummy
        call sendbyte
        mov dx , 3FDH ; Line Status Register
        in al , dx 
        test al , 1
        JZ WaitJoin ;Not Ready
        call sendName
        call RecieveName
        ret  

        second:
        mov values,dummy
        call sendbyte
        call RecieveName
        call sendName


        ret
ExchangeNames endp

RecieveName proc near
       wrongS:
        call RecieveByte
        cmp valueR,dummy
        jne wrongS

        mov di,offset othername
        mov cx,16
        RN:
        inc di
        call RecieveByte
        mov al,valuer
        mov byte ptr [di],al
        loop RN

        ret
RecieveName endp

SendName proc near
;----------Sending Seed---------------

        mov di,offset myname 
        mov cx,16
        SN:
        inc di
        mov al,byte ptr [di]
        mov valueS,al
        call SendByte
        loop SN

        ret
SendName endp


RecieveByte proc ;wait until you recieve
    ;Check that Data is Ready
    endRec:
    mov dx , 3FDH ; Line Status Register
    in al , dx 
    test al , 1
    JZ endRec ;Not Ready

    ;If Ready read the VALUE in Receive data register
    mov dx , 03F8H
    in al , dx
    mov ValueR , al
    ret
RecieveByte endp

SendByte proc ;wait until you make sure it's sent

    mov dx , 3FDH ; Line Status Register

    endSend: 
    In al , dx ;Read Line Status
    test al , 00100000b
    JZ endSend

    ;If empty put the VALUE in Transmit data register
    mov dx , 3F8H ; Transmit data register
    mov al,valueS
    out dx , al

    ret
SendByte endp

SendDataController proc
    mov bx,BytesNum
    mov DI,offset Controller_Data
   SendN:
    mov al,[di]
    mov values,al
    call SendByte
    inc DI
    dec BX
    jnz sendN
    ret
SendDataController endp



RecieveDataOther proc
    rcO: ;consume bytes till valid byte 
    call RecieveByte
    cmp ValueR,dummy
    je Recieve
    jmp rcO
    
  Recieve: 
    mov bx,BytesNum-1
    mov DI,offset Controller_Data+1
   RecN:
    call RecieveByte
    mov al,valuer
    mov [di],al
    inc DI
    dec BX
    jnz RECN
    ret
RecieveDataOther endp

RecieveDataController proc
    rc:         ;consume bytes till valid byte 
    call RecieveByte
    cmp ValueR,dummy
    je Recieve
    jmp rc
    call RecieveByte
    mov al,valuer
    mov btn,al
    ret
RecieveDataController endp

ConnectionInit proc near
        mov dx,3fbh ; Line Control Register
        mov al,10000000b ;Set Divisor Latch Access Bit
        out dx,al ;Out it
        ;---------------------9600 bit per second----------------
        ;--------------------wordLength 8bits  ------------------ 
        mov dx,3f8h
        mov al,01h
        out dx,al
        mov dx,3f9h
        mov al,00h
        out dx,al

        mov dx,3fbh
        mov al,00011111b
        out dx,al
        ret

ConnectionInit endp
;-------------------Procedures-------------------------------------------------
DrawStatus proc near
    mov dh,3
    mov dl,2
    mov ah ,02    
    int 10h 
    mov al,level
    and ah,0
    call print

    mov dh,0
    mov dl,5
    mov ah ,02    
    int 10h
    ShowMessage MyName+2
    
    mov dh,0
    mov dl,5
    add dl,MyName+1
    add dl,2
    mov ah ,02    
    int 10h
    mov ax,score
    add ax,bounus
    call PRINT    

    cmp timepan1,0
    je nopanc1
    mov X,iceW/2+3
    mov Y,iceH
    mov W,iceW
    mov H,iceH
    mov imgofs,offset ice
    call draw
nopanc1:
    cmp CHARACTERTODRAW,0
    je notZ1
    mov X,iceW+ZstatusW+6
    mov Y,ZstatusH
    mov W,ZstatusW
    mov H,ZstatusH
    mov imgofs,offset Zstatus
    call draw
notZ1:
    mov dh,1
    mov dl,5
    mov ah ,02    
    int 10h
   ShowMessage OtherName+2
    mov dh,1
    mov dl,5
    add dl,OtherName+1
    add dl,2
    mov ah ,02    
    int 10h
    
    mov ax,score2
    add ax,bounus2
   
    call PRINT    
    
    cmp timepan2,0
    je nopanc2
    mov X,iceW/2+3
    mov Y,iceH*2
    mov W,iceW
    mov H,iceH
    mov imgofs,offset ice
    call draw
nopanc2:
    cmp CHARACTERTODRAW2,0
    je notZ2
    mov X,iceW+ZstatusW+6
    mov Y,ZstatusH*2
    mov W,ZstatusW
    mov H,ZstatusH
    mov imgofs,offset Zstatus
    call draw
notZ2: 

    ret
DrawStatus endp 
;-------------------Procedures-------------------------------------------------
DrawStatus2 proc near
    mov dh,3
    mov dl,2
    mov ah ,02    
    int 10h 
    mov al,level
    and ah,0
    call print

    mov dh,0
    mov dl,5
    mov ah ,02    
    int 10h
    ShowMessage othername+2
    
    mov dh,0
    mov dl,5
    add dl,othername+1
    add dl,2
    mov ah ,02    
    int 10h
    mov ax,score
    add ax,bounus
    call PRINT    

    cmp timepan1,0
    je nopanc12
    mov X,iceW/2+3
    mov Y,iceH
    mov W,iceW
    mov H,iceH
    mov imgofs,offset ice
    call draw
nopanc12:
    cmp CHARACTERTODRAW,0
    je notZ12
    mov X,iceW+ZstatusW+6
    mov Y,ZstatusH
    mov W,ZstatusW
    mov H,ZstatusH
    mov imgofs,offset Zstatus
    call draw
notZ12:
    mov dh,1
    mov dl,5
    mov ah ,02    
    int 10h
   ShowMessage myName+2
    mov dh,1
    mov dl,5
    add dl,myName+1
    add dl,2
    mov ah ,02    
    int 10h
    
    mov ax,score2
    add ax,bounus2
   
    call PRINT    
    
    cmp timepan2,0
    je nopanc22
    mov X,iceW/2+3
    mov Y,iceH*2
    mov W,iceW
    mov H,iceH
    mov imgofs,offset ice
    call draw
nopanc22:
    cmp CHARACTERTODRAW2,0
    je notZ22
    mov X,iceW+ZstatusW+6
    mov Y,ZstatusH*2
    mov W,ZstatusW
    mov H,ZstatusH
    mov imgofs,offset Zstatus
    call draw
notZ22: 

    ret
DrawStatus2 endp 

GameWithLogic proc near
    
    call ChooseLevel
    call CurrentTime
    pop START_SECOND

    CALL RESETGAME
;----------Sending Seed---------------
    mov values,dummy
    call SendByte

    mov ax,start_second
    mov valueS,al
    call SendByte

    mov ax,start_second
    mov valueS,ah
    call SendByte

    mov ah,level
    mov valueS,ah
    call SendByte
    
    jmp DRAWFRAME
    
GameLoop:
    cmp level,1 
    jg nofric
        call Friction
        call Friction2
    nofric:
        call land
        call land2

        cmp TimePan2,0
        je NoPan2
            add flags,1
            mov AccelerationX2,0 ;So he is not gaining velocity while freezing 
            dec TimePan2
        jg checkPlayer1

        NoPan2: 
            call UpdataPos2
        
    checkPlayer1:
        cmp TimePan1,0
        je NoPan
            add flags,2
            mov AccelerationX,0 ;So he is not gaining velocity while freezing 
            dec TimePan1
        jg DrawFrame

        NoPan: 
            call UpdataPos  
        
        call scroll
        call scroll2

    DrawFrame:
        ChangeGraphicsMode ; to clear the screen
        call DrawGameFrame
        call DrawStatus
        
        call Collision
        call ApplySpecialpower
        call ApplySpecialpower2
        call CalculateScore
        call CalculateScore2
        
        call CheckEndGame
        

        ;---------------Score to Send-------------------    
        mov ax,score
        add ax,bounus
        mov Tscore1,ax

        mov ax,score2
        add ax,bounus2
        mov Tscore2,ax

        ;--------------Flags------------------

        ;-----------Delay--------------------
        mov cx, 0h
        mov dx,Delay
        mov ah,86h
        int 15h

        call SendDataController
        cmp StartChat ,0
        je nochat     
            call chatingame
            mov StartChat ,0
nochat:

        cmp GameWinner,3
        jne seeIFWinner
            ret
        seeIFWinner: 
        cmp GameWinner,0
        je ResetFlags ;no GameWinner
            
            call drawEndGameScreen
            ret
        
ResetFlags: mov flags,0
notValid: 
        call RecieveByte
        cmp valueR,dummy
        jne notValid
       
        ;Valid 
        
        call RecieveByte
        mov al,valueR ;other player input
        mov btn,al
        

    mov ah,1
    int 16h          
    jnz HaveInput ;no key pressed
    jmp SERVEP2

HaveInput:
    mov ah,0
    int 16h  
    
    cmp al,1bh ;ESC
    jne Mmm
    mov gameWinner,3
    MMM:


    cmp AH ,16D
    jne nchat
    jmp ChatIn 
    nchat:
    
    cmp ah,4bh
    je Left

    cmp ah,4dh
    je Right

    cmp ah,39h
    je Space
    ;---------------------Player 2----------------------------
    ServeP2:
    
   

    cmp btn,'l';left
    jne n1
    jmp Left2
    n1:
    cmp btn,'r' ;right
    jne n2
    jmp Right2
    n2:
    cmp btn,'j';jump
    jne n3
    jmp jump2
    n3:
    cmp btn,'e';jump
    jne n4
    mov gameWinner,3
    n4:
    cmp btn,'t';chat
    jne n5
    jmp ChatIn
    n5:

    jmp GAMELOOP ;loop until Esc pressed

    Left:
        cmp XDir,-1
        je wasLeft
        mov ax,velocityX ;to gain extra velocity when changing the direcction
        imul CHANGEDIR
        mov velocityx,ax
        mov XDir,-1 
    wasLeft:
        add AccelerationX,XPar
        jmp ServeP2 
    Right: 
        cmp XDir,1
        je wasRight
        mov ax,velocityX
        imul CHANGEDIR
        mov velocityx,ax
        mov XDir,1
    wasRight:
        add AccelerationX,XPar
        jmp ServeP2 
    Space: 
        cmp ISLANDED,0
        je ENDSPACE ;to prevent more than jump

        mov ax,VelocityX
        cmp ax,0
        jg ps
            imul CHANGEDIR
        ps:
        cmp ax,MinJumpVel ;can't Jump
        jle EndSpace

        Mov ISLANDED,0
        MOV VelocityY,jmpStrength

        mul VelocityY
        add	VelocityY,AX	;Vy=	|Vx|*Jmps
        cmp velocityY,minJmpStrength
        jge endSpace
        mov velocityY,minJmpStrength

    endSpace:
        jmp ServeP2 
    ;----------------------------------Player 2--------------------
    Left2:
        cmp XDir2,-1
        je wasLeft2
        mov ax,velocityX2 ;to gain extra velocity when changing the direcction
        imul CHANGEDIR
        mov velocityx2,ax
        mov XDir2,-1 
    wasLeft2:
        add AccelerationX2,XPar
        jmp GAMELOOP 
    Right2: 
        cmp XDir2,1
        je wasRight2
        mov ax,velocityX2
        imul CHANGEDIR
        mov velocityx2,ax
        mov XDir2,1
    wasRight2:
        add AccelerationX2,XPar
        jmp GAMELOOP 
    jump2: 
        cmp ISLANDED2,0
        je ENDjump2 ;to prevent more than jump

        mov ax,VelocityX2
        cmp ax,0
        jg ps2
            imul CHANGEDIR
        ps2:
        cmp ax,MinJumpVel ;can't Jump
        jle Endjump2

        Mov ISLANDED2,0
        MOV VelocityY2,jmpStrength

        mul VelocityY2
        add	VelocityY2,AX	;Vy=	|Vx|*Jmps
        cmp velocityY2,minJmpStrength
        jge endjump2
        mov velocityY2,minJmpStrength

    endjump2:
        jmp GAMELOOP 
    ChatIN:  
        mov StartChat,1
        jmp GAMELOOP 
    ;------------------For both players
    Escape:
    ret
GameWithLogic endp

Game proc near

        CALL RESETGAME
NOINPUT:
        mov btn,dummy

    GameLoop2:
        mov valueS,dummy
        call SendByte
        mov al,btn
        mov valueS,al
        call SendByte

       
        call RecieveDataOther
        
        cmp  StartChat,0
        je nochat2
            call chatingame 
            mov startchat,0

        nochat2:
        ;REmoving Cards -------------------------
        mov di ,offset Cards
        mov al,Cardc1
        mov bl ,6 
        mul bl 
        add di,ax
        cmp [di+2],6
        jne nochangeincha
        mov charactertodraw,1
        nochangeincha:
        mov word ptr  [di+4],0 
        
        ;REmoving Cards -------------------------
        mov di ,offset Cards
        mov al,Cardc2
        mov bl ,6 
        mul bl 
        add di,ax
        cmp [di+2],6
        jne nochangeincha2
        mov charactertodraw2,1
        nochangeincha2:
        mov word ptr  [di+4],0 
        
        ;-----------------Score-------------
        mov ax,TSCORE1
        mov score ,ax
        mov ax,TSCORE2
        mov score2,ax 
        ;-----------Status-------------------
        mov al,2
        and al,flags
        mov timepan1,al
        mov al,1
        and al,flags
        mov timepan2,al

        jmp nohelp
            NoINHelper:jmp NoInput 
        nohelp:
        ;--------- if We have GameWinner------------
        cmp GAMEWINNER,3
        jne winner?
        ret
        winner?:
        cmp GAMEWINNER,0
        je noGWinner
        call drawEndGameScreen2
        ret
noGWinner:
        ;-------------------Flags-----------------
        test flags ,01000000b
        jz noScroll
        call ScrollNoCheck
        noScroll:
        ChangeGraphicsMode ; to clear the screen
        call DrawGameFrame
        call DrawStatus2

        ;-----------Delay--------------------
        mov cx, 0h
        mov dx,Delay/2
        mov ah,86h
        int 15h

        
        mov ah,1
        int 16h          
        jz NoINHelper

        mov ah,0
        int 16h  
        

        cmp aH,16D
        je chatin2

        cmp ah,4bh
        je Leftp2

        cmp ah,4dh
        je Rightp2

        cmp ah,39h
        je Spacep2
        
        cmp al,1bh ;ESC
        je escp2

        mov btn,'n' ;nothing 
        jmp GAMELOOP2

        Leftp2:
            mov btn,'l' 
            jmp GAMELOOP2
        Rightp2: 
            mov btn,'r'  
            jmp GAMELOOP2
        Spacep2: 
            mov btn,'j'
            jmp GAMELOOP2
        escp2:
            mov btn,'e'
            jmp GAMELOOP2
        chatin2:
            mov btn,'t'
            jmp GAMELOOP2
        ret
Game endp

DrawGameFrame proc near
        call DrawBG
        call DrawCards
    ;---------Chracter 1-------------------
        mov AX,characterX
        mov X,AX
        mov AX,characterY
        mov Y,AX
    cmp charactertodraw,1
    je z1
        mov AL,imgW
        mov W,AL
        mov AL,imgH
        mov H,AL
        mov bx,offset [img]
        mov imgofs,bX
        jmp drawc1
    z1:
        mov AL,imgzW
        mov W,AL
        mov AL,imgzH
        mov H,AL
        mov bx,offset [imgZ]
        mov imgofs,bX

    drawC1:
        call Draw
    ;---------Chracter 2-------------------
        mov AX,characterX2
        mov X,AX
        mov AX,characterY2
        mov Y,AX
    cmp charactertodraw2,1
    je z2
        mov AL,img2W
        mov W,AL
        mov AL,img2H
        mov H,AL
        mov bx,offset [img2]
        mov imgofs,bX
        jmp drawc2
    z2:
        mov AL,imgzW
        mov W,AL
        mov AL,imgzH
        mov H,AL
        mov bx,offset [imgZ]
        mov imgofs,bX

    drawC2:
        call Draw
        ret
DrawGameFrame endp

Scroll proc near 
    cmp characterY,scrollheight
    jl validscroll
        ret
    validScroll:
    add flags,01000000b
    mov cx,stpsn
    mov di,offset stps+4
    nextStep:
	add	word	ptr	[di]	, scrollspeed
    ; sub currentheight,scrollspeed
    add di,6
    loop nextstep

    add characterY,scrollSpeed
    add characterY2,scrollSpeed
    ret
Scroll endp

ScrollnoCheck proc near 

    mov cx,stpsn
    mov di,offset stps+4
    nextStepS:
	add	word	ptr	[di]	, scrollspeed
    ; sub currentheight,scrollspeed
    add di,6
    loop nextstepS

    ret
ScrollnoCheck endp
CalculateScore proc
    mov ax,Sheight-StepHeight
    sub ax,characterY
    mov di, scorefactor
    mul di ;height*score factor
    mov di,ax
    call CurrentTime 
    pop bx
    sub bx,start_second ;bx=time difference+1
    inc bx
    mov ax,di
    div bx
    cmp ax,score
    jle nochange
    mov score,ax
    nochange:
    ret 
CalculateScore endp 


UpdataPos proc near ;Go Down TIll Hitting Step 
            ;-----------Update X
            mov ax,accelerationX
            imul XDir
            Add ax,VelocityX
            cmp ax,MAXVEL
            jg SetMaxVelRight
            cmp ax,-1*MAXVEL
            Jl SetMaxVelLeft
            jmp Cont
SetMaxVelRight:  
            mov ax,MAXVEl
            jmp Cont
SetMaxVelLeft:  
            mov ax,MAXVEl*-1

Cont:       mov VelocityX,ax
            add characterX,ax
            mov accelerationX,0
          
            ;------Updatey
            cmp ISLANDED,1
            je NoYMove
                Sub VelocityY,gravity
                mov ax,VelocityY
                sub characterY,ax
    NoYMove:
            ;------------Hitting the Walls(Screen End)
            Cmp characterX,imgW/2
            jle HitLeft
            Cmp characterX,SWidth-imgW/2
            jge HitRight
            jmp EndUpdate

HitLeft:    mov CharacterX,imgW/2
            jmp Stop

HitRight:   mov CharacterX,SWidth-imgW/2
            
Stop:       mov AccelerationX,0
            mov VelocityX,0            
EndUpdate:  ret

UpdataPos endp 

Land proc near
            cmp IsLanded,0 ;AlreadyLanded
            je NotLanded
			call CheckOnGround
NotLanded:
            cmp VelocityY,0 ;movingUp
            jge EndLand 
			

            mov DI,SHeight-1
    mov ax,0d00h
			mov bh,0
			mov CX,characterX
			mov DX,CharacterY
CheckLanding:
			Inc DX 
			cmp DX,DI 
			je EndLand
			int 10h
			cmp al,StepsColor
			jne Checklanding
			mov VelocityY,0
			mov CharacterY,DX
           	mov ISLANDED,1

EndLand:    ret
Land endp 


Friction proc near
            cmp IsLanded,0
            je EndF ; No friction in air
            
            cmp velocityX,0
            je EndF ;Doesn't Move
            jg FrictR
            jl FrictL

    FrictR: Sub velocityx,Fric
            jg endF ;Direction Did't change
            mov velocityx,0 ;Stop
            jmp endF

    FrictL: add velocityx,Fric
            cmp velocityx,0 
            jle EndF ;Direction Did't change
            mov velocityx,0 ;Stop
 EndF:      ret
Friction endp

CheckOnGround proc Near
			mov ax,0d00h
			mov bh,0
			mov CX,characterX
			mov DX,CharacterY
			Inc DX 
			int 10h
			cmp al,StepsColor
			je OnGround
			mov ISLANDED,0 ;let gravity take efferct 
OnGround: 	ret
CheckOnGround endp 

;------------------Draws the character ar CharacterX,y------------------------------
Draw proc near
            MOV CX,X

            mov ah,0
            mov al,W
            mov bl,2
            div bl
            mov ah,0
        
			Add CX,ax
			mov DX,Y ; Set Start postion of drawing Character Pivot will be in middle buttom of him 

			mov SI,CX
            mov ah,0 
            mov al,W
    
            mov bx,ax
     
			Sub SI,ax ;horizontal End Left
			mov BP,DX 

            mov ah,0 
            mov al,H
    
			Sub BP,AX ;vertical End Up
			
			MOV DI,imgofs ;set DI to itterate over pixels
			MOV ah,0ch 
    			
		DrawPx:
			cmp al,0
			je DontDraw
			int 10h
	DontDraw:
			inc DI
			mov al,[DI]
			DEC CX ;Go lef
			Cmp CX,SI
			JNZ DrawPX
			DEC DX ;Go up
			Cmp DX,BP
			JZ EndDraw
			add CX,bx
			jmp DrawPX
			
	EndDraw:
			ret
Draw endp
Collision proc near
    cmp characterY,SHeight-stepinbetween ;no collision on first step 
    jge endColl
    cmp characterY2,SHeight-stepinbetween ;no collision on first step 
    jge endColl

    mov bx,characterX
    mov ax,characterX
    add ax,velocityX
    cmp xdir,1 
    je goingRight
    ;----------Going left ax-margin <[chracter2X]< bx+margin

    cmp characterX2,ax
    jl endColl
    cmp characterX2,bx
    jg endColl
    ;there is collision
    jmp checkYcol

goingright:
    ;----------Going Right bx <[chracter2X]< ax
    cmp characterX2,ax
    jg endColl
    cmp characterX2,bx
    jl endColl
    ;there is collision
    jmp checkYcol

    endColl:
    ret

checkYCol:
    ; characterY2-imgh <characterY <characterY2+imgh    
    mov ax,characterY2    
    add ax,imgh    
    sub ax,3
    mov bx,characterY2
    sub bx,imgh
    add bx,3
    cmp characterY,ax
    jg endColl
    cmp characterY,bx
    jl endColl
    ;--------------------Collision-----------------------------------------------------------
    ;-------------------player with lessY | Velocity |  falls ----------------
    mov ax,velocityY
    cmp ax,0
    jge psY
        imul changedir
    PSY:
    mov bx,ax ;bx= yVel -> 1

    mov ax,velocityY2
    cmp ax,0
    jge psY2
        imul changedir
    PSY2:
    mov cx,ax ;cx= yVel -> 2

    mov ax,velocityX
    cmp ax,0
    jge psX
        imul changedir
    PSX:
    add bx,ax 

    mov ax,velocityX2
    cmp ax,0
    jge psX2
        imul changedir
    PSX2:
    add cx,ax 


    cmp cx,bx
    jg player1Falls
    mov ISLANDED2,0
    jmp endColl
player1Falls:
    mov ISLANDED,0
Collision endp
;Card x offset ,Color(type),isused
;Card x offset ,Color,isused,type
ApplySpecialpower proc near
    mov si,offset Stps - 6
    mov bx,offset Cards - 6
    push CardsNum
    check:
    add si,6
    add bx,6
    mov ax,[si] + 2
    add ax,[bx]
    mov dx,ax
    add dx,Cardlength
    CheckX:cmp characterX,ax
    je CheckY
    inc ax
    cmp ax,dx
    jne CheckX
    dec CardsNum
    cmp CardsNum,0
    je return
    jmp check

    appplyandhide:
    mov cx,0
    mov [bx] + 4,cx
   
    mov ax,bx
    sub ax,offset Cards ;hide from othe pc also
    mov bl,6
    div bl
    mov Cardc1,al

    return:pop CardsNum
    ret
    
    CheckY:mov cx,[si] + 4
    cmp characterY,cx
    je one
    dec CardsNum
    cmp CardsNum,0
    je return
    jne check
    one:
    cmp [bx] + 4,0
    je return
    mov cx,[bx] + 2
    cmp cx,1
    jne two
    mov characterX,SWidth/2
    mov characterY,SHeight-StepHeight
    jmp appplyandhide

    two:cmp cx,2
    jne three
    mov cx,[si] + 8
    mov characterX,cx
    add CharacterX,70
    mov cx,[si] + 10
    mov characterY,cx
    jmp appplyandhide

    three:cmp cx,3
    jne four
    mov TimePan1,75
    jmp appplyandhide

    four:cmp cx,4
    jne five
    add BOUNUS,card4Bounus
    jmp appplyandhide

    five:cmp cx,5
    jne six
    mov TimePan2,75
    jmp appplyandhide

    six:cmp cx,6
    jne return
    mov CharacterToDraw,1
    jmp appplyandhide
    
ApplySpecialpower endp


PRINT PROC  near


    cmp ax,09
    jg startPrint
        add al,48d
        mov dl,al
        mov ah,02h 
        int 21h 
    ret
    ;initilize count
    startPrint: 
    mov cx,0 
    mov dx,0 
    label1: 
        ; if ax is zero 
        cmp ax,0 
        je print1       
          
        ;initilize bx to 10 
        mov bx,10         
          
        ; extract the last digit 
        div bx                   
          
        ;push it in the stack 
        push dx               
          
        ;increment the count 
        inc cx               
          
        ;set dx to 0  
        xor dx,dx 
        jmp label1 
    print1: 
        ;check if count  
        ;is greater than zero 
        cmp cx,0 
        je exit
          
        ;pop the top of stack 
        pop dx 
          
        ;add 48 so that it  
        ;represents the ASCII 
        ;value of digits 
        add dx,48
          
        ;interuppt to print a 
        ;character 
        mov ah,02h 
        int 21h 
          
        ;decrease the count 
        dec cx 
        jmp print1 
exit: 
ret 
PRINT ENDP 
CheckEndGame proc near
        cmp characterY,SHeight
        jg player2Wins
        cmp characterY2,SHeight
        jg player1Wins
        jmp endCheck
Player1Wins:
        mov GAMEWINNER,1
        jmp endCheck
Player2Wins:
        add flags,00110000b
        mov GameWinner,2
        

endCheck:
ret
CheckEndGame endp
DrawEndGameScreen proc near
        mov  ah,0
        mov  al,3h
        int  10h
        mov dh,MessY
        mov dl,MessX
        mov ah ,02    
        int 10h
        ShowMessage GameOver 
        mov dh,MessY+3
        mov dl,MessX
        mov ah ,02    
        int 10h
        ShowMessage WinnerIS
        cmp GameWinner,1
        jne winneris2
        showMessage MyName+2
        jmp contS
        winneris2:
        showMessage OtherName+2

contS:  mov dh,MessY+6
        mov dl,MessX
        mov ah ,02    
        int 10h
        ShowMessage pressAny 

        mov ah,0
        int 16h
    ret
DrawEndGameScreen endp
DrawEndGameScreen2 proc near
        mov  ah,0
        mov  al,3h
        int  10h
        mov dh,MessY
        mov dl,MessX
        mov ah ,02    
        int 10h
        ShowMessage GameOver 
        mov dh,MessY+3
        mov dl,MessX
        mov ah ,02    
        int 10h
        ShowMessage WinnerIS
        cmp GameWinner,1
        jne winneris22
        showMessage otherName+2
        jmp contS2
        winneris22:
        showMessage MyName+2

contS2:  mov dh,MessY+6
        mov dl,MessX
        mov ah ,02    
        int 10h
        ShowMessage pressAny 

        mov ah,0
        int 16h
    ret
DrawEndGameScreen2 endp

GenerateSteps proc near
    mov cx,stpsn
    mov si,[stps+4] ;Height of current step
    mov di,offset stps +6 ;start from second step

    GenStep:
    ;----------------len -x - y----------------------------------------------
    ;-----------Generate length----------------------------------------------
    call rand
    mov bx,sWidth/4
    mov ax,randseed
    xor dx, dx
    div bx

    add dx,swidth/4
    mov [di],dx ;Take mod

    ;------------Generate X -----------------------------------------
    call rand
    mov ax,randseed
    mov bx,sWidth
    sub bx,[di] ;remove the length
	add	bx,1	;avoid	0	division
    xor dx, dx
    div bx
    add di,2
    mov [di],dx
    ;--------------Generate Y----------------------------------------
    sub si,stepinbetween
    add di,2
    mov [di],si
    add di,2
    loop GENSTEP

    ret
GenerateSteps endp
GenerateCards proc near
    mov si,offset stps
    mov di,offset cards+4
    mov cx,stpsn-1
    
    ;----------Generata random card position
    genNextCard:
    add si,6
    add di,2
    call rand
    mov ax,randseed
    mov bx,[si];step len
    sub bx,cardlength
    xor dx, dx
    div bx
    mov [di],dx ;Take mod

    add di,2
    call rand
    mov ax,randseed
    mov bx,cardTypes ;can only have 6 cards for now 
    xor dx, dx
    div bx
    mov [di],dx ;Take mod

    add di,2
    cmp dx,0
    je noCardsGen
    mov [di],1
    jmp contgen
    nocardsGen:
    mov [di],0
    contGen:
    loop genNextCard
    ret
GenerateCards endp 

RESETGAME PROC NEAR
    ;---------------------Game reset----------------------------
    ;---------------------Player 1------------------------------
    mov characterx,swidth/3
    mov charactery,SHeight-StepHeight
    mov score,0
    mov bounus,0
    mov charactertodraw,0

    mov xdir,0
    mov velocityx,0
    mov velocityy,0
    mov accelerationX,0
    mov Islanded,1
    mov timepan1,0
    ;---------------------Player 2------------------------------
    mov characterx2,2*swidth/3
    mov charactery2,SHeight-StepHeight
    mov score2,0
    mov bounus2,0
    mov charactertodraw2,0
    mov xdir2,0
    mov velocityx2,0
    mov velocityy2,0
    mov accelerationX2,0
    mov Islanded2,1
    mov timepan2,0
    mov randseed,0abcdh
    mov GameWinner,0

    mov di,offset stps
    mov [di],Swidth
    mov word ptr[di+2],0
    mov  word ptr [di+4],SHeight-StepHeight
    call GenerateSteps
    call GenerateCards
    ret
RESETGAME ENDP

DrawBG proc near
    mov DI,offset Stps-6
    mov al,StepsColor
    MOV ah,0ch ;draws pixel 
	MOV bh,0 
    mov bl,stpsN
DrawStep:
    add Di,6
    cmp word ptr [DI+4],0
    jle enddrawBG
    cmp word ptr [DI+4],SHeight
    jl insideScreen
    jmp GONEXTSTEP
    insideScreen:
    mov Cx,[DI]
    Add CX,[DI+2] ; right end
    mov Dx,[DI+4]
    add Dx,StepHeight
    DrawStepPoint:
    int 10h
    Dec Cx 
    cmp Cx,[DI+2] ;is left Edge 
    jge DrawStepPoint
    add cx,[DI]
    Dec Dx
    cmp Dx,[DI+4]
    jge DrawStepPoint
   goNextStep:
    dec bl
    jnz DRAWSTEP
endDrawBG:
    ret
DrawBG endp 

;Card xoffset ,Color,isused,type
;Card xoffset ,Color(type),isused
DrawCards proc near
    mov ah,0ch
    mov si,offset Stps - 6
    mov bx,offset Cards - 6
    push CardsNum
    drawcard:
    add si,6
    add bx,6
    cmp [bx]+4,0
    je drawnextcard
    mov al,byte ptr [bx] + 2
    mov cx,[si] + 2
    add cx,[bx]
    mov di,cx
    add di,Cardlength
    mov dx,[si] + 4
    sub dx,Cardlength
    DrawVerticalLine2:
    int 10H
    inc cx
    cmp cx,di
    jnz DrawVerticalLine2
    inc dx
    mov cx,[si] + 2
    add cx,[bx]
    cmp dx,[si] + 4
    jnz DrawVerticalLine2
    drawnextCard:
    dec CardsNum
    cmp CardsNum,0
    jnz drawcard
    pop CardsNum
    return2: ret
    
DrawCards endp

CurrentTime proc near
        pop si

        MOV AH, 02CH                   ; get the current system time
        INT 21H                       ;Ch=Hours , cl=minutes
                                      ;DH= Seconds 
        ;(Hours%12*60+minutes)*60+seconds
        mov al,ch
        mov ah,0
        mov bl, 12d
        div bl ;Hours % 12 in ah
        ;------To save seconds as dh will change 
        mov ch,dh
        
        mov al,ah
        mov ah,0
        mov bx ,60d
        mul bl ;ax =hours%12*60

        mov dl,cl
        mov dh,0
        add ax,dx
        mul bx ;(Hours%12*60+minutes)*60
        mov cl,ch
        mov ch,0
        add ax,cx  ;(Hours%12*60+minutes)*60+seconds

        push ax ;the result 
        push si
        ret
CurrentTime endp

rand proc near
    mov ax,start_second
    add randseed,ax
    mul randseed
	add	randseed,ax	;	to	cause overflow
	add	randseed,ax	;	to	cause overflow
    ret
rand endp
;-----------------------Main Menu procedures ------------------------

GetUserChoice proc near
 getChoice:
    cmp choosen,0
    jge pos
    mov choosen,0
pos:
    cmp choosen,6
    jle valid
    mov choosen,6
valid:
    ;--------Remove Old choice------------
    mov ah,9 ;Display
    mov bh,0 ;Page 0
    mov al," " ;Letter 
    mov cx,1h ;1 times
    mov bl,00h ;Green (A) on white(F) background
    int 10h
    ;---------Draw new One---------------------
    mov dh,MessY
    add dh,choosen
    mov dl,MessX-2
    mov ah ,02    
    int 10h

    mov ah,9 ;Display
    mov bh,0 ;Page 0
    mov al,">" ;Letter 
    mov cx,1h ;1 times
    mov bl,0Ah ;Green (A) on white(F) background
    int 10h

CheckKey:
    mov ah,1
    int 16h          
    jz CheckKey ;no key pressed        

    mov ah,0
    int 16h  
    
    cmp ah,48h
    je up

    cmp ah,50h
    je down

    cmp aL,0dH
    je EnterBtn

    up:
        Sub CHOOSEN,3
        JMP GetChoice
    Down:
        add CHOOSEN,3
        JMP GetChoice
    EnterBtn:
        ret
GetUserChoice endp
DrawMainMenu proc near
    mov  ah,0
    mov  al,3h
    int  10h
    
    mov dh,MessY
    mov dl,MessX
    mov ah ,02    
    int 10h
    showMessage GameMessage
    mov dl,MessX
    mov ah ,02    
    mov dh,MessY+3
    int 10h
    showMessage ChatMessage
    mov dl,MessX
    mov ah ,02    
    add dh,MessY+6
    int 10h
    showMessage Exitmes
    ret
DrawMainMenu endp
usernamescreen PROC
	               mov  ah,0
	               mov  al,3h
	               int  10h
        
	               mov  ah,2
	               mov  dh,8
	               mov  dl,25
	               int  10h
	               mov  ah,9
	               mov  dx,offset entermsg1
	               int  21h

	               mov  ah,2
	               mov  dh,16
	               mov  dl,25
	               int  10h
	               mov  ah,9
	               mov  dx,offset entermsg2
	               int  21h



	               enteragain1:mov  ah,2
	               mov  dh,10
	               mov  dl,30
	               int  10h
	               mov  ah,0ah
	               mov  dx ,offset MyName
	               int  21h

                   call validation1
				   cmp faultinMyName,1
				   je enteragain1

	

	               ret
     
usernamescreen ENDP

validation1 proc Near
								 mov cl,MyName + 1 
								 mov si,offset MyName
								 mov bx,offset OtherName
								 add si,2
								 mov ah,0
								 mov al,[si]
								 cmp al,32 ;space char
								 je fault
								 inc si
								 dec cl
								 cmp cl,0
								 je return1
								 loop1:
								 mov al,[si]
								 range1:cmp al,65
								 jae range2
								 cmp al,32 ;space char
								 jne fault
								 inc si
								 dec cl
								 cmp cl,0
								 je return1
								 jmp loop1

								 range2:cmp al,90
								 ja range3
								 inc si
								 dec cl
								 cmp cl,0
								 jne loop1
								
								 range3:cmp al,97
								 jae range4
								 jmp fault

								 range4:cmp al,122
								 ja fault
								 inc si
								 dec cx
								 cmp cx,0
								 jne loop1
								 jmp nofault
								 
								 fault:mov faultinMyName,1
								 jmp return1

								 nofault:mov faultinMyName,0
								 return1:ret

								 validation1 endp


;------------------------------------------Chating Proc-------------
chat proc near 

	; make the design of the screen
	               call Chatscreen
	               call serialcom
                   ret
chat endp
  
	;description
serialcom PROC

	               mov  csx , 5
	               mov  csy , 1
	               mov  crx,5
	               mov  cry,13

	; initializing the UART (baud rate, parity, data bits, stop bits,�).
              
	               mov  dx,3fbh                	; Line Control Register
	               mov  al,10000000b           	;Set Divisor Latch Access Bit
	               out  dx,al                  	;Out it
	               mov  dx,3f8h
	               mov  al,0ch
	               out  dx,al
	               mov  dx,3f9h
	               mov  al,00h
	               out  dx,al
	               mov  dx,3fbh
	               mov  al,00011011b
	               out  dx,al

	; sending part
	AGAIN:         
	;get key pressed
	               mov  ah,1
	               int  16h
	               mov  value,al
	               jz   chk
	               mov  ah,00h
	               INT  16h
	;check if key = enter
	              
	; check if key = backspace
	               cmp  value,08h
	               jz   backspaceS
	               cmp  value,13
	               jz   enterS
	;check if key = ESC
	               cmp  value,1bh
	               jz   endd
	               mov  dx , 3FDH              	; Line Status Register
	               In   al , dx                	;Read Line Status
	               test al , 00100000b
	               JZ   CHK                    	;Not empty
	               call setcs
	               add  csx,1
	;display the char
	               mov  ah,9
	               mov  al,value
	               mov  bl,0ah
	               mov  cx,1
	               int  10h
	B:             mov  dx , 3F8H              	; Transmit data register
	               mov  al,VALUE
	               out  dx , al

	; reciving part
	CHK:           
	               mov  dx , 03FDH             	; Line Status Register
	               in   al , dx
	               test al , 1                 	;Check that Data is Ready
	               JZ   AGAIN                  	;Not Ready
	               mov  dx , 03F8H
	               in   al , dx
	               mov  VALUE , al

	               cmp  value ,08h
	               jz   backspaceR

	               cmp  value ,13
	               jz   enterR

	               call setcr
	               add  crx,1
	;display the char
	               mov  ah,9
	               mov  al,value
	               mov  bl,0ah
	               mov  cx,1
	               int  10h
	               jmp  again

	endd:          
	            

	               ret

	backspaceS:    
	               cmp  csx,0
	               je   chk
                   
	               sub  csx,1
	               call setcs
	               mov  ah,2
	               mov  dl,' '
	               int  21h
	               call setcs
	               jmp  b
	              

	enterS:        
	               add  csy,1
	               mov  csx,0
	               call setcs
	               jmp  b
                
    
	enterR:        
	               add  cry,1
	               mov  crx,0
	               call setcr
	               jmp  again

	backspaceR:    
                   
	               sub  crx,1
	               call setcr
	               mov  ah,2
	               mov  dl,' '
	               int  21h
	               call setcr
	               jmp  again
    
serialcom ENDP

	; make the design of the screen
Chatscreen proc near

	               mov  ah,0
	               mov  al,3h
	               int  10h


	               mov  bl,11
	               mov  cx,0

	L:             
	               mov  ah,2
	               mov  dh,bl
	               mov  dl,cl
	               int  10h
	               mov  ah,2
	               mov  dl,'-'
	               int  21h
	               inc  cx
	               cmp  cx,80
	               jnz  L

	               cmp  bl,23
	               jz   e
	               mov  bl,23
	               mov  cx,0
	               jmp  L

	e:             
	               mov  ah,2
	               mov  dh,0
	               mov  dl,1
	               int  10h
	               mov  ah,9
	               mov  dx,offset MyName+2
	               int  21h
	               mov  ah,9
                   
                  

	               mov  ah,2
	               mov  dh,12
	               mov  dl,1
	               int  10h
	               mov  ah,9
	               mov  dx,offset OtherName+2
	               int  21h
	            

	               mov  ah,2
	               mov  dh,24
	               mov  dl,1
	               int  10h
	               mov  ah,9
	               mov  dx,offset endmsg1
	               int  21h
	               mov  ah,9
	               mov  dx,offset OtherName
	               int  21h
	               mov  ah,9
	               mov  dx,offset endmsg2
	               int  21h
	               ret

Chatscreen endp

	;set cursor position of sending region
setcs PROC near



	                      cmp                csx,80
	                      jz                 nls

	nsp:                  
	                      mov                ah,2
	                      mov                dh,csy
	                      mov                dl,csx
	                      int                10h


	                      ret
	nls:                  
	              
	                      mov                csx,0
	                      add                csy,1
	                      				   cmp csy,11
					jz scrollss
	            	jmp  nsp
 scrollss:
 mov ah,6
mov al,0
mov cl,0
mov ch,1
mov dl,79
mov dh,1
int 10h
mov ah,6
mov al,1
mov cl,0
mov ch,1
mov dl,79
mov dh,10
int 10h
sub csy,1
jmp nsp
setcs ENDP

	;set cursor position of reciving region
setcr PROC near
	                      cmp                crx,80
	                      jz                 nlr

	nrp:                  
	                      mov                ah,2
	                      mov                dh,cry
	                      mov                dl,crx
	                      int                10h


	                      ret
	nlr:                  
	              
	                      mov                crx,0
	                      add                cry,1
	                       cmp cry,23
					jz scrollrr
	            	jmp  nrp
 scrollrr:
 mov ah,7
mov al,0
mov cl,0
mov ch,13
mov dl,79
mov dh,13
int 10h
mov ah,6
mov al,1
mov cl,0
mov ch,13
mov dl,79
mov dh,22
int 10h
sub cry,1
jmp nrp
ret
setcr ENDP
;-------------------------------------Player 2--------------
Scroll2 proc near 
    cmp characterY2,scrollheight
    jl validscroll2
        ret
    validScroll2:
    add flags,01000000b
    mov cx,stpsn
    mov di,offset stps+4
    nextStep2:
	add	word	ptr	[di]	, scrollspeed
    ; sub currentheight,scrollspeed
    add di,6
    loop nextstep2

    add characterY,scrollSpeed
    add characterY2,scrollSpeed
    ret
Scroll2 endp
CalculateScore2 proc
    mov ax,Sheight-StepHeight
    sub ax,characterY2
    mov di, scorefactor
    mul di ;height*score factor
    mov di,ax
    call CurrentTime 
    pop bx
    sub bx,start_second ;bx=time difference+1
    inc bx
    mov ax,di
    div bx
    cmp ax,score2
    jle nochange2
    mov score2,ax
    nochange2:
    ret 
CalculateScore2 endp 


UpdataPos2 proc near ;Go Down TIll Hitting Step 
            ;-----------Update X
            mov ax,accelerationX2
            imul XDir2
            Add ax,VelocityX2
            cmp ax,MAXVEL
            jg SetMaxVelRight2
            cmp ax,-1*MAXVEL
            Jl SetMaxVelLeft2
            jmp Cont2
SetMaxVelRight2:  
            mov ax,MAXVEl
            jmp Cont2
SetMaxVelLeft2:  
            mov ax,MAXVEl*-1

Cont2:      mov VelocityX2,ax
            add characterX2,ax
            mov accelerationX2,0
          
            ;------Updatey
            cmp ISLANDED2,1
            je NoYMove2
                Sub VelocityY2,gravity
                mov ax,VelocityY2
                sub characterY2,ax
    NoYMove2:
            ;------------Hitting the Walls(Screen End)
            Cmp characterX2,img2W/2
            jle HitLeft2
            Cmp characterX2,SWidth-img2W/2
            jge HitRight2
            jmp EndUpdate2

HitLeft2:    mov CharacterX2,img2W/2
            jmp Stop2

HitRight2:   mov CharacterX2,SWidth-img2W/2
            
Stop2:      mov AccelerationX2,0
            mov VelocityX2,0            
EndUpdate2:  ret

UpdataPos2 endp 

Land2 proc near
            cmp IsLanded2,0 ;AlreadyLanded
            je NotLanded2
			call CheckOnGround2
NotLanded2:
            cmp VelocityY2,0 ;movingUp
            jge EndLand2 
			

            mov DI,SHeight-1
    mov ax,0d00h
			mov bh,0
			mov CX,characterX2
			mov DX,CharacterY2
CheckLanding2:
			Inc DX 
			cmp DX,DI 
			je EndLand2
			int 10h
			cmp al,StepsColor
			jne Checklanding2
			mov VelocityY2,0
			mov CharacterY2,DX
           	mov ISLANDED2,1

EndLand2:    ret
Land2 endp 


Friction2 proc near
            cmp IsLanded2,0
            je EndF2 ; No friction in air
            
            cmp velocityX2,0
            je EndF2 ;Doesn't Move
            jg FrictR2
            jl FrictL2

    FrictR2: Sub velocityx2,Fric
            jg endF2 ;Direction Did't change
            mov velocityx2,0 ;Stop
            jmp endF2

    FrictL2: add velocityx2,Fric
            cmp velocityx2,0 
            jle EndF2 ;Direction Did't change
            mov velocityx2,0 ;Stop
 EndF2:      ret
Friction2 endp

CheckOnGround2 proc Near
			mov ax,0d00h
			mov bh,0
			mov CX,characterX2
			mov DX,CharacterY2
			Inc DX 
			int 10h
			cmp al,StepsColor
			je OnGround2
			mov ISLANDED2,0 ;let gravity take efferct 
OnGround2: 	ret
CheckOnGround2 endp 
ApplySpecialpower2 proc near
    mov si,offset Stps - 6
    mov bx,offset Cards - 6
    push CardsNum
    check2:add si,6
    add bx,6
    mov ax,[si] + 2
    add ax,[bx]
    mov dx,ax
    add dx,Cardlength
    CheckX2:cmp characterX2,ax
    je CheckY2
    inc ax
    cmp ax,dx
    jne CheckX2
    dec CardsNum
    cmp CardsNum,0
    je return3
    jmp check2

    appplyandhide2:
   
   
    mov cx,0
    mov [bx] + 4,cx
    mov ax,bx
    sub ax,offset Cards ;hide from othe pc also
    mov bl,6
    div bl
    mov Cardc2,al
    return3:
         pop CardsNum
    ret
    
    CheckY2:mov cx,[si] + 4
    cmp characterY2,cx
    je one2
    dec CardsNum
    cmp CardsNum,0
    je return3
    jne check2
    one2:cmp [bx] + 4,0
    je return3
    mov cx,[bx] + 2
    cmp cx,1
    jne two2
    mov characterX2,SWidth/2
    mov characterY2,SHeight-StepHeight
    jmp appplyandhide2

    two2:cmp cx,2
    jne three2
    mov cx,[si] + 8
    mov characterX2,cx
    add CharacterX2,70
    mov cx,[si] + 10
    mov characterY2,cx
    jmp appplyandhide2

    three2:cmp cx,3
    jne four2
    mov TimePan2,75
    jmp appplyandhide2

    four2:cmp cx,4
    jne five2
    add BOUNUS2,card4Bounus
    jmp appplyandhide2

    five2:cmp cx,5
    jne six2
    mov TimePan1,75
    jmp appplyandhide2

    six2:cmp cx,6
    jne return3
    mov CharacterToDraw2,1
    jmp appplyandhide2
    
ApplySpecialpower2 endp

chatingame PROC
	                            mov ch,myname +1
                          add ch , 9
	               mov  csx , ch
	                      mov                csy , 0
	                        mov ch, othername+1
                            add ch ,9
	                    mov  crx,ch
	                      mov   cry,1

	; ; initializing the UART (baud rate, parity, data bits, stop bits,�).
              
	;                       mov                dx,3fbh                           	; Line Control Register
	;                       mov                al,10000000b                      	;Set Divisor Latch Access Bit
	;                       out                dx,al                             	;Out it
	;                       mov                dx,3f8h
	;                       mov                al,0ch
	;                       out                dx,al
	;                       mov                dx,3f9h
	;                       mov                al,00h
	;                       out                dx,al
	;                       mov                dx,3fbh
	;                       mov                al,00011011b
	;                       out                dx,al

	; sending part
	GAGAIN:               
	;get key pressed
	                      mov                ah,1
	                      int                16h
	                      mov                value,al
	                      jz                 Gchk
	                      mov                ah,00h
	                      INT                16h
	;check if key = enter
	              
	; check if key = backspace
	                      cmp                value,08h
	                      jz                 GbackspaceS
	                      cmp                value,13
	                      jz                Gchk
	;check if key = ESC
	                      cmp                value,1bh
	                      jz                 Gendd
	                      mov                dx , 3FDH                         	; Line Status Register
	                      In                 al , dx                           	;Read Line Status
	                      test               al , 00100000b
	                      JZ                 GCHK                              	;Not empty
	                      call               Gsetcs
	                      add                csx,1
	;display the char
	                      mov                ah,9
	                      mov                al,value
	                      mov                bl,0ah
	                      mov                cx,1
	                      int                10h
	GB:                   mov                dx , 3F8H                         	; Transmit data register
	                      mov                al,VALUE
	                      out                dx , al

	; reciving part
	GCHK:                 
	                      mov                dx , 03FDH                        	; Line Status Register
	                      in                 al , dx
	                      test               al , 1                            	;Check that Data is Ready
	                      JZ                 GAGAIN                            	;Not Ready
	                      mov                dx , 03F8H
	                      in                 al , dx
	                      mov                VALUE , al

	                      cmp                value ,1bh
	                      jz                 endchatingame

	                      cmp                value ,08h
	                      jz                 GbackspaceR

	                      cmp                value ,13
	                      jz                 Gagain

	                      call               Gsetcr
	                      add                crx,1
	;display the char
	                      mov                ah,9
	                      mov                al,value
	                      mov                bl,0ah
	                      mov                cx,1
	                      int                10h
                          
	                      jmp                Gagain

	Gendd:                
	                      mov                dx , 3F8H                         	; Transmit data register
	                      mov                al,1bh
	                      out                dx , al
	               
	                      ret

	GbackspaceS:          
	                      cmp                csx,0
	                      je                 Gchk

	                        mov ah,myname+1
                    add ah,8
	                      cmp                csx,ah
	                      je                 Gchk
                   
	                      sub                csx,1
	                      call               Gsetcs
	                      mov                ah,2
	                      mov                dl,' '
	                      int                21h
	                      call               Gsetcs
	                      jmp                Gb

	GbackspaceR:          
                   
	                       mov ah,othername+1
                   add ah,8
	                      cmp                crx,ah
	                      je                 Gchk
	                      sub                crx,1
	                      call               Gsetcr
	                      mov                ah,2
	                      mov                dl,' '
	                      int                21h
	                      call               Gsetcr
	                      jmp                GAGAIN
	endchatingame:        
	                      ret
    
chatingame ENDP

Gsetcs PROC near



	                      cmp                csx,80
	                      jz                 Gnls

	Gnsp:                 
	                      mov                ah,2
	                      mov                dh,0
	                      mov                dl,csx
	                      int                10h


	                      ret
	Gnls:                 
	              
	                       mov ch,myname+1
                  add ch,9
	                      mov                csx,ch
	                      mov                ah,6
	                      mov                al,0
	                      mov                cl,10
	                      mov                ch,0
	                      mov                dl,79
	                      mov                dh,0
	                      int                10h
	                      jmp                Gnsp
	

	                      ret
Gsetcs ENDP

	;set cursor position of reciving region
Gsetcr PROC near
	                      cmp                crx,80
	                      jz                 Gnlr

	Gnrp:                 
	                      mov                ah,2
	                      mov                dh,1
	                      mov                dl,crx
	                      int                10h


	                      ret
	Gnlr:                 
	              


	                     mov ch,othername+1
                  add ch,9
	                      mov                crx,ch
	                      mov                ah,7
	                      mov                al,0
	                      mov                cl, 10
	                      mov                ch,1
	                      mov                dl,79
	                      mov                dh,1
	                      int                10h
	                     
	            
	                     
	                      jmp                Gnrp

Gsetcr ENDP

;  Collision2 proc near
;     cmp characterY2,SHeight-stepinbetween-StepHeight ;no collision on first step 
;     jge endColl2
;     cmp characterY,SHeight-stepinbetween-StepHeight ;no collision on first step 
;     jge endColl2
;     mov bx,characterX2
;     mov ax,characterX2
;     add ax,velocityX2
;     cmp xdir2,1 
;     je goingRight2
;     ;----------Going left ax-margin <[chracter2X]< bx+margin

;     cmp characterX,ax
;     jl endColl2
;     cmp characterX,bx
;     jg endColl2
;     ;there is collision
;     jmp checkYcol2

; goingright2:
;     ;----------Going Right bx <[chracter2X]< ax

;     cmp characterX,ax
;     jg endColl2
;     cmp characterX,bx
;     jl endColl2
;     ;there is collision
;     jmp checkYcol2

;     endColl2:
;     ret

; checkYCol2:
;     ; characterY2-imgh <characterY <characterY2+imgh    
;     mov ax,characterY    
;     add ax,imgh 
;     sub ax,3
;     mov bx,characterY
;     sub bx,imgh
;     add bx,3
;     cmp characterY2,ax
;     jg endColl2
;     cmp characterY2,bx
;     jl endColl2
;     ;--------------------Collision-----------------------------------------------------------
;     ;-------------------player with lessY | Velocity |  falls ----------------
;     mov ax,velocityY2
;     cmp ax,0
;     jge psY3
;         imul changedir
;     PSY3:
;     mov bx,ax ;bx= yVel -> 1

;     mov ax,velocityY
;     cmp ax,0
;     jge psY4
;         imul changedir
;     PSY4:
;     mov cx,ax ;cx= yVel -> 2

;     mov ax,velocityX2
;     cmp ax,0
;     jge psX3
;         imul changedir
;     PSX3:
;     add bx,ax 

;     mov ax,velocityX
;     cmp ax,0
;     jge psX4
;         imul changedir
;     PSX4:
;     add cx,ax 


;     cmp cx,bx
;     jg player1Falls2
;     mov ISLANDED,0
;     jmp endColl2
; player1Falls2:
;     mov ISLANDED2,0
; Collision2 endp
end main
