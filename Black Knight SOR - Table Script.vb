'********************************************************************************************
'*																							*
'*           ____  _            _      _  __      _       _     _   						*
'*          | __ )| | __ _  ___| | __ | |/ /_ __ (_) __ _| |__ | |_ 						*
'*          |  _ \| |/ _` |/ __| |/ / | ' /| '_ \| |/ _` | '_ \| __|						*
'*          | |_) | | (_| | (__|   <  | . \| | | | | (_| | | | | |_ 						*
'*          |____/|_|\__,_|\___|_|\_\ |_|\_\_| |_|_|\__, |_| |_|\__|						*
'*                                                  |___/           						*
'*           ____                       _    ___   __   ____                  				*
'*          / ___|_      _____  _ __ __| |  / _ \ / _| |  _ \ __ _  __ _  ___ 				*
'*          \___ \ \ /\ / / _ \| '__/ _` | | | | | |_  | |_) / _` |/ _` |/ _ \				*
'*           ___) \ V  V / (_) | | | (_| | | |_| |  _| |  _ < (_| | (_| |  __/				*
'*          |____/ \_/\_/ \___/|_|  \__,_|  \___/|_|   |_| \_\__,_|\__, |\___|				*
'*                                                                 |___/      				*
'*																							*
'*																							*
'********************************************************************************************

'Some DOF Config by Outhere
'101, Left Flipper
'102, Right Flipper
'103, Left Slingshot
'104, 
'105, Right Slingshot
'106, 
'107, Bumper Left
'108, Bumper Center
'109, Bumper Right
'110, Ball Release
'111, 
'112, Shield Up
'113, SW60 Kick Out
'114 
'115, 
'116 
'117 
'118, Shield Down
'119, Kicker007
'120,
'121, CastleVUK2
'122 
'123 
'124 
'136, Knocker

'DOF light config by Kongedam
'200 Start Button
'201 Magna save Button
'202 Magna save Electrical Arcing Effects
'205 Ball ready to shoot
'220 Letter Rage: R***
'221 Letter Rage: *A**
'222 Letter Rage: **G*
'223 Letter Rage: ***E
'224 Letter Rage: RA**
'225 Letter Rage: R*G*
'226 Letter Rage: R**E
'227 Letter Rage: RAG*
'228 Letter Rage: R*GE
'229 Letter Rage: *AG*
'230 Letter Rage: *AGE
'231 Letter Rage: **GE
'232 RAGE Flash
'300 Attract mode Effect
'400 Bumper 1
'401 Bumper 2
'402 Bumper 3
'600 "SKILL" letters flash, Splash Effect Center to Up and Down, 403 Splash Effect Center to Up and Down
'601 1 Locked Flashing
'602 2 Locked Flashing
'603 MULTI Flashing, Vertically Moving Dashes, Vertically Moving Dashes
'604 Multiball Beacon
'600 Knight Arm
'605 Shield and left droptaget locked 
'700 Award replay


Option Explicit
Randomize

Const BallMass = 1.7    ' standard ball mass in JP's VPX Physics 3.0
Const Ballsize = 50

LoadCoreFiles
Sub LoadCoreFiles
    On Error Resume Next
    ExecuteGlobal GetTextFile("core.vbs")
    If Err Then MsgBox "Can't open core.vbs"
    ExecuteGlobal GetTextFile("controller.vbs")
    If Err Then MsgBox "Can't open controller.vbs"
    On Error Goto 0
End Sub

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  Player Options
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X 
'************************************************************************************************************************
'************************************** YOU CAN MODIFY HERE *************************************************************

Const bpgcurrent = 3		 	' Ball Per Game - see also variable "Ballremaining"  'HERE YOU CAN CHANGE THEN NUMBER OF BALL (3 or 5)				
'Const bpgcurrent = 5		 	' Ball Per Game - see also variable "Ballremaining"  'HERE YOU CAN CHANGE THEN NUMBER OF BALL (3 or 5)				
Const ReplayAt = 120000000		' Replay At 120,000,000



Const pDMD=2					
Const pBackglass=1	


'*********************************** HERE YOU CAN ADJUST THE VOLUME *****************************************************
dim vovol: vovol = 50 ' set the volume of the voice over callouts
dim vsvol: vsvol = 50 ' set the volume of the cinematic videos
dim sndknightvol: sndknightvol = 50
Dim sndtrk:sndtrk = 1 ' 0 = British 90s Rock, 1 = Goblet of Fire Theatrical Score
dim sndtrkvol: sndtrkvol = vsvol 
dim SongVolume:SongVolume = 1/(101-vovol)   ' 1 is full volume, but I set it quite low to listen better the other sounds :) Don't touch this line!!!

'Const pDMD=1
'Const pBackglass=2

'************************************************************************************************************************

Const BallSaverTime = 15

Const cGameName = "bksor"
Const DifficultyRageCountMystery=1
'Const directory = "HKEY_CURRENT_USER\SOFTWARE\Visual Pinball\Controller\"
'Const DOFBell = 4
'Const DOFChimes = 3
'Const DOFContactors = 1
'Const DOFDropTargets = 9
'Const DOFFlippers = 7
'Const DOFGear = 5
'Const DOFKnocker = 2
'Const DOFOff = 0
'Const DOFOn = 1
'Const DOFPulse = 2
'Const DOFShaker = 6
'Const DOFTargets = 8
Const ExtraBallAfterMonsterDefeatedValue = 1
Const MaxMultiballs = 10
Const MaxMultiplier = 6 
Const MaxPlayers = 4
Const MissionTime = 65       'Time for a mission in Seconde (The default time is 65)
Const MissionTimeLastChance=30
Const myVersion = "3.77"
Const numberfont = "CCDuskTillDawnRisenUpW00"
Const numberfont2 = "DSEG14 Classic"
Const pTopper=0
Const pPlayfield=3
Const pMusic=4
Const pAudio=5
Const pCallouts=6
Const pBackGlass2=7
Const pPopSW71=8
Const pPopSW72=9
Const pPop=10
'***********************
Const pBigLine=2
Const pDMDBlank=0
Const pDMDTypeFULL=2
Const pDMDTypeLCD=0
Const pDMDTypeReal=1
Const pGame=15
Const pOvervid=14
Const pScores=1
Const pTargerLetters=5
Const pThreeLines=3
Const pTwoLines=4
'Const SongVolume = 0.15 ' 1 is full volume, but I set it quite low to listen better the other sounds :)      See below...

Const TableName = "Sword of Rage"
Const typefont = "CCDuskTillDawnRisenUpW00"
Const typefont2 = "DSEG14 Classic"
Const typefontbold = "CCDuskTillDawnRisenUpWBold"
Const zoombgfont = "Magic School One" ' needs to be an outline of the zoomfont
Const zoomfont = "Magic School One"






'dmdType




	turnoffrules = 0 ' change to 1 to take off the backglass helper rules text during a game
	turnondmd = 2 ' 0 = off, 1=regular dmd size 2 = double tall size // this now uses pinup. No ultradmd anymore
	ballrolleron = 1 ' set to 0 to turn off the ball roller if you use the "c" key in your cabinet
	gibase = 60 'brightness of your gis
'-------------------------------------------------->>>> Sound
'***************************************************************
'******************* Define Variable  **************************
'***************************************************************
Dim a,b,c,d
Dim f,g,h
Dim activemode(4)
Dim AddChestJackpot, AddScoreChestJackpot
Dim AddScoreBlackCastle, AddScoreBlackCastleLoop, AddScoreBlackCastleTotal(4), AddScoreBlackCastleTotalEndGame(4)
Dim AddscoreCatapult, AddscoreCatapultBonus, AddscoreCatapultBonusAfterMulti, AddscoreCatapultTotal(4), AddscoreCatapultTotalEndGame(4)
Dim AddScoreDeepFreeze, AddScoreDeepFreezeAwarded, AddScoreDeepFreezeAwardedBonusLoop, AddScoreDeepFreezeFrozen, AddScoreDeepFreezeFrozenBonusLoop, AddScoreDeepFreezeTotal(4), AddScoreDeepFreezeTotalEndGame(4)
Dim AddScoreLoopBonus
Dim AddScoreMoltenFire, AddScoreMoltenFireBonusLoop, AddScoreMoltenFireTotal(4), AddScoreMoltenFireTotalEndGame(4)
Dim AddScoreMudBog, AddScoreMudBogBonusLoopSealed, AddScoreMudBogBonusLoopSliced, AddScoreMudBogSealed, AddScoreMudBogSliced, AddScoreMudBogTotal(4), AddScoreMudBogTotalEndGame(4)
Dim AddScoreSandWorm, AddScoreSandWormLoop, AddScoreSandWormTotal(4), AddScoreSandWormTotalEndGame(4)
Dim AddScoreSuperLanes, AddScoreSuperOrbits, AddScoreSuperPops, AddScoreSuperSlings, AddScoreSuperSpinner, AddScoreSuperTargets
Dim AddScoreSW82
Dim AddScoreTripleKnightChallenge, AddScoreTripleKnightChallengeGoldRoom, AddScoreTripleKnightChallengeKnightHited, AddScoreTripleKnightChallengeTotal(4), AddScoreTripleKnightChallengeTotalEndGame(4)
Dim AddScoreWarHurryUp, AddScoreWarHurryUpTotal(4), AddScoreWarHurryUpTotalEndGame(4)
Dim AddScoreWickedCavern, AddScoreWickedCavernLoop, AddScoreWickedCavernTotal(4), AddScoreWickedCavernTotalEndGame(4)
Dim AddTimeForMission(4)			'--> Change to continue mission ---> remove  ...If the ball is drained the AddTimeForMission=0 for the next player see : Sub EndOfBallComplete()			
Dim AddTimeToDrainForDisplayModeTotal
Dim animation_start_and_finish_flag
Dim arcbld:arcbld=1
Dim arcblu:arcblu=2
Dim arcbrd:arcbrd=3
Dim arcbru:arcbru=4
Dim arctld:arctld=5
Dim arctlu:arctlu=6
Dim arctrd:arctrd=7
Dim arctru:arctru=8
Dim audioknight
Dim audionoise
Dim AutoFireTime:AutoFireTime=0
dim awamor
Dim B2SController, B2SOn, B2SOnALT
Dim BackGroundFileIs, BackGroundFolderIs, BackGroundLenghtIs
Dim Ball:Ball=0
Dim BallInCatapult(4)
Dim BallInLocker
Dim BallInPlay
Dim BallKeptInLocker
Dim BallRemainingPrevision
Dim ballrolleron
Dim BallSaveAvailable(4)
Dim BallSavedFlag
Dim BallsInHole
Dim BallsInLock
Dim BallsOnPlayfield
Dim BallsOnPlayfieldPrevision
Dim BallsRemaining(4)
Dim BaseBonus
Dim bAttractMode
Dim bAutoPlunger
Dim bballfirstball
Dim bBallInPlungerLane
Dim bBallSaverActive
Dim bBallSaverActiveL13
Dim bBallSaverReady
Dim bBonusHeld
Dim BeginCurrentMissionFlag
Dim bExtraBallWonThisBall
Dim bFreePlay
Dim bGameInPlay
Dim bInstantInfo
Dim BIP:BIP = 0
Dim bJustStarted
Dim BlackCastle(4)
Dim BlackCastleDefeated(4)
Dim BlackCastleMissionHitStart(4)
Dim BlackKnightRetro(4)
Dim bMultiBallMode
Dim bMusicOff
Dim bMusicOn
Dim bOnTheFirstBall
dim Bonus
Dim BonusHeldPoints(4)
Dim BonusMultiplier(4)
Dim BonusPoints(4)
Dim BouleAngle
Dim BouleHited' Used when the shield is opened for start a mission - normal state the "boule" is lock. If the ball is hit the "boule" turn for reject the ball if the boule is hit a second time ????
Dim bromconfig
Dim broomdone:broomdone = 0
Dim bSkillshotReady
Dim bSuperJackpotMode
Dim bumplvl(4)
Dim BurningSands(4)
Dim BurningSandsCount, BurningSandsHit, BurningSandsLightMovementNumber
Dim BurningSandsDefeated(4)
Dim BypassVideo
Dim canstart:canstart=0
Dim Catapult_jackpot_collected, Catapult_Mode_Total_score(4), Catapult_Super_Jackpot_Multi, CatapultModeFlag
dim catapult_lock_is_lit(4)
Dim ChangeSuperMode
Dim cineon:cineon = 0
Dim circlein:circlein=9
Dim circleout:circleout=10
Dim clockleft:clockleft=11
Dim clockright:clockright=12
Dim ColorRoundLight
Dim Combos
Dim CommentDisplayed
Dim compteuri
Dim compteuriSW70Hit, compteuriSW70Idle, compteuriSW70Death, compteuriSW71Hit, compteuriSW71Idle, compteuriSW71Death, compteuriSW72Hit, compteuriSW72Idle, compteuriSW72Death
Dim compteurPng
Dim compteurStructured
Dim Controller
Dim CounOrbitsFlag
dim Count
Dim CountSW45used(4) 					'after the ball go to SW45 4x --> The SW41 wakeup	
Dim Credits
Dim curBallX   : curBallX=0
Dim curBallY   : curBallY=0
Dim curLine1Color: curLine1Color=pLine1Color  'can change later
Dim curLine2Color: curLine2Color=pLine2Color  'can change later
Dim curLine3Color: curLine3Color=pLine3Color  'can change later
Dim currentcombo(4)
Dim currenteyepos:currenteyepos=6
Dim CurrentMissionFlag(4)			'If the ball is drained the CurrentMissionFlag=0 for the next player see : Sub EndOfBallComplete()			
Dim CurrentPlayer
Dim currentqueue
Dim curScore
Dim DBCredits
Dim DeepFreeze(4)
Dim DeepFreezeDefeated(4)
Dim DeepFreezeMissionHitStart(4), DeepFreezeTargetRedHit
Dim DelayknightLamp
Dim DelayVideoAttract
Dim delvl(4)
Dim deshit(4)
Dim desnd:desnd = 0
Dim diagdl:diagdl=13
Dim diagdr:diagdr=14
Dim diagul:diagul=15
Dim diagur:diagur=16
Dim dmdnote:dmdnote="2-shortnote.mp4"
Dim dmdver : dmdver = PuPDMDDriverType
	if dmdver = 2 Then
	Else
		dmdver = 1 
	end if
'Dim DOFeffects(9)
Dim dragonjacks(4)
Dim dragonlock(4)
Dim dragonmod: dragonmod=1
	if nofunmode = 0 Then
		dragonmod = 1
	Else
		dragonmod = 0
	end if
Dim Drains
Dim EnableBallControl:	EnableBallControl = false 'Change to true to enable manual ball control (or press C in-game) via the arrow keys and B (boost movement) keys			
Dim EnableRetractPlunger		
Dim EventNumold
Dim ExtraBallAfterMonsterDefeated(4)
Dim ExtraBallsAwards(4)
Dim ExtraBallsIsLit
Dim eyeCurX    : eyeCurX=0 
Dim eyeCurY    : eyeCurY=0 
Dim eyeFollowS : eyeFollowS=8  'ms timer interval of eye follow speed try 5-20 range
DIM eyeMoveToX : eyeMoveToX=0
DIM eyeMoveToY : eyeMoveToY=0
Dim eyeMoving  : eyeMoving=false
Dim eyepos:eyepos=6
Dim eyespin:eyespin = 0
Dim eyewiggle:eyewiggle=0
Dim fanld:fanld=18
Dim fanlu:fanlu=19
Dim fanrd:fanrd=20
Dim fanru:fanru=21
Dim FEffect:FEffect = 0
Dim FEStep:FEStep = 0
Dim Filenamepng
Dim finalflips
Dim FireAnimationBump
Dim firepos:firepos = 0
Dim FlashState(100), FadingLevel(100), FlashSpeedUp(100), FlashSpeedDown(100), FlashMin(100), FlashMax(100), FlashLevel(100), FlashRepeat(100)
Dim Gamerover_flag	'Stop music after end the endmusic					
Dim gibase
Dim gistatus:gistatus = 0
Dim gobjp:gobjp =0
Dim goblets(4)
Dim HasPup:HasPuP = True
Dim hatch1h:hatch1h=22
Dim hatch1v:hatch1v=23
Dim hatch2h:hatch2h=24
Dim hatch2v:hatch2v=25
dim help:help = 1 ' switch to 0 to turn off the helpful callouts about rules
Dim helppos : helppos = RndNum(1,5)
Dim HideOverlay
Dim HighScore(5)
Dim HighScoreBlackCastleChampionName, HighScoreBlackCastleChampion
Dim HighScoreBonusChampionName, HighScoreBonusChampion
Dim HighScoreBurningSandsChampionName, HighScoreBurningSandsChampion
Dim HighScoreCatapultMBChampionName, HighScoreCatapultMBChampion
Dim HighScoreComboChampionName, HighScoreComboChampion
Dim HighScoreDeepFreezeChampionName, HighScoreDeepFreezeChampion
Dim HighScoreKnightChampionName, HighScoreKnightChampion
Dim HighScoreLoopChampionName, HighScoreLoopChampion
Dim HighScoreMoltenFireChampionName, HighScoreMoltenFireChampion
Dim HighScoreMudBogChampionName, HighScoreMudBogChampion
Dim HighScoreName(5)
Dim HighScoreSuperChampionName, HighScoreSuperChampion
Dim HighScoreTripleKnightsMBChampionName, HighScoreTripleKnightsMBChampion
Dim HighScoreWarChampionName, HighScoreWarChampion
Dim HighScoreWickedCavernChampionName, HighScoreWickedCavernChampion
Dim hs4,hs3,hs2,hs1,hs0,hsn4,hsn3,hsn2,hsn1,hsn0
Dim hsbModeActive:hsbModeActive = False
Dim hschecker:hschecker = 0
Dim hsCurrentDigit
Dim hsCurrentLetter
Dim hsdigit:hsdigit = 1
Dim hsEnteredDigits(3)
Dim hsEnteredName
Dim hsletter:hsletter = 1
Dim hsLetterFlash
Dim hsValidLetters
Dim IconSuperTargets, IconSuperSlings, IconSuperLanes, IconSuperPops, IconSuperOrbits, IconSuperSpinner
Dim inamode:inamode = 0
Dim inbumps:inbumps = 0
Dim incurses:incurses = 0
Dim indeath:indeath = 0
Dim ingob:ingob = 0
Dim inmermulti:inmermulti = 0
Dim inmode
Dim InProgress: Inprogress=False
Dim inquid:inquid = 0
Dim introposition
Dim introtime:introtime = 0
Dim irecall
Dim ishield'used to count the number of move of the shield (normaly 4 at the start )
Dim istoprotating
Dim Jackpot
Dim Knight_challenge(4)
Dim Knight_challenge_flag 'Knight challenge Active or Not
Dim knight_challenge_jackpot, Knight_challenge_phase, KnightRemaining
Dim knightchallenge
Dim KnightLamp(4)
Dim L103color(4), L103State(4)
Dim L29State, L35State, L41State, L47State, L53State, L59State, L66color(4), L66State(4), L72color(4), L72State(4), L79color(4), L79State(4), L85color(4), L85State(4), L91color(4), L91State(4), L98color(4), L98State(4)
Dim LaneBonus
Dim LastChanceBallToLock, LastChanceButtonPush(4), LastChanceCatapultMode, LastChanceCatapultMulti, LastChanceChronoStart(4), LastChanceClearTable, LastChanceDrainUncompleted, LastChanceFlag, LastChanceStart, LastChanceSuccess
Dim lasthit
Dim LastSwitchHit
Dim lastvocall:lastvocall=""
Dim ldown:ldown = 0
Dim LeftGateTWcount
Dim light
Dim LightChecked3x
Dim Lightcounter
Dim Lightcounter2
dim lightstateflag
Dim LightType(12), LightObjectColor(12)
Dim looktimer:looktimer = 0
Dim Loops
Dim lrhits(4)
Dim MagnaSaveFlag(4)
Dim MatchRandom
Dim mazecall:mazecall = 0
Dim mazejacks(4)
Dim mazelocks(4)
Dim mb1,mb2,mb3,mb4,mb5 'variables multiball
Dim mBalls2Eject
Dim merlinsecond(4)
Dim merlock1full:merlock1full = 0
Dim merlock2full:merlock2full = 0
Dim merlock3full:merlock3full = 0
Dim mermultispins(4)
Dim MessageCredits
Dim middleih:middleih=27
Dim middleiv:middleiv=28
Dim middleoh:middleoh=29
Dim middleov:middleov=30
Dim Mission
Dim MissionRandom
Dim mMagnetSave
Dim modeeb(4)
Dim modescompleted(4)
Dim modesecs:modesecs = 0
Dim MoltenFire(4)
Dim MoltenFireDefeated(4)
Dim msg1
Dim msg2
Dim MudBog(4)
Dim MudBogDefeated(4)
dim Multiball
dim Multiplier
Dim Musicball
Dim musicpriority
Dim MusicSelected
Dim MysteryFlag
Dim NewBallInLock
Dim NewRecord
Dim NextOrbitHit:NextOrbitHit = 0
dim nofunmode:nofunmode = 0 ' switch to 1 if you hate fun or just have a low end pc. it'll turn off the animations
Dim noskipper:noskipper=0
Dim notenow:notenow = dmdnote
dim notinus : notinus = 0 ' set this if you're not from the USA and it'll make sure your backglass numbers have commas
Dim NumberOfHitForStartMission(4)
Dim NumberOfMissioncomplete(4)
Dim objShell
Dim OldBackGroundFileIs
Dim OldBlackCastleDefeated
Dim OldBurningSandsDefeated
Dim OldDeepFreezeDefeated
Dim OldGiState:	OldGiState = -1   'start witht the Gi off					
Dim OldMoltenFireDefeated
Dim OldMudBogDefeated
Dim OldMusicSelected
Dim oldplaylist
Dim OldstateHideOverlay
Dim OldWickedCavernDefeated
Dim OrbitBonus
Dim OrbitFlag
Dim osbactive:osbactive = 2 'set to 0 for off, 1 for only player 1 to be sent, 2 for all scores to be sent.
Dim osbdefinit:osbdefinit = "" ' your default initials to use 
Dim osbid:osbid ="" ' your orbital scoreboard login name
Dim osbkey:osbkey="" ' your orbital scoreboard api key
Dim osbtemp:osbtemp = osbdefinit
Dim osbtempscore:osbtempscore = 0
Dim Overlaydirectory
Dim OverlaySelected
DIM pAttractBetween: pAttractBetween=2000 '1 second between calls to next attract page
DIM pAttractReset:pAttractReset=-1
Dim pbumps(4)
DIM pCurAttractPos: pCurAttractPos=0
Dim pDMDCurPage: pDMDCurPage= 0     'default page is empty.
Dim pDMDCurPriority: pDMDCurPriority =-1
Dim pDMDDefVolume: pDMDDefVolume = 0   'default no audio on pDMD
Dim pDMDlastchk: pDMDLastchk= -1    'performance of updates
DIM pDMDVideoPlaying: pDMDVideoPlaying=false
Dim pGameName       : pGameName="bksor"    'pupvideos foldername, probably set to cGameName in realworld
Dim pInAttract : pInAttract=false   'pAttract mode
Dim PlayerCheckCurrent
Dim playerspins(4)
Dim PlayersPlayingGame
Dim playingsj:playingsj = 0
Dim PleaseWait:PleaseWait=True				'launch puppack completely before to play		
Dim pLine1
Dim pLine1Ani
Dim pLine1Color : pLine1Color=8454143  
Dim pLine2
Dim pLine2Ani
Dim pLine2Color : pLine2Color=8454143
Dim pLine3
Dim pLine3Ani
Dim pLine3Color :  pLine3Color=8454143
Dim plungerIM 'used mostly as an autofire plunger
Dim pNumLines
Dim PopupMessage
Dim potionlevel(4)
Dim potions(4)
Dim potready(4)
Dim ppblit(4)
Dim priority
Dim PriorityReset:PriorityReset=-1
Dim ps13(4)
Dim ps53(4)
Dim ps71(4)
Dim pslr1(4)
Dim pslr2(4)
Dim pslr3(4)
Dim pslr4(4)
Dim psrr1(4)
Dim psrr2(4)
Dim psrr3(4)
Dim psrr4(4)
Dim pt1(4)
Dim pt10(4)
Dim pt11(4)
Dim pt12(4)
Dim pt13(4)
Dim pt14(4)
Dim pt15(4)
Dim pt16(4)
Dim pt17(4)
Dim pt18(4)
Dim pt19(4)
Dim pt2(4)
Dim pt20(4)
Dim pt21(4)
Dim pt22(4)
Dim pt23(4)
Dim pt24(4)
Dim pt25(4)
Dim pt26(4)
Dim pt27(4)
Dim pt28(4)
Dim pt29(4)
Dim pt3(4)
Dim pt30(4)
Dim pt31(4)
Dim pt32(4)
Dim pt33(4)
Dim pt34(4)
Dim pt35(4)
Dim pt36(4)
Dim pt37(4)
Dim pt4(4)
Dim pt5(4)
Dim pt6(4)
Dim pt7(4)
Dim pt72(4)
Dim pt8(4)
Dim pt9(4)
dim PuPDMDDriverType: PuPDMDDriverType=2   ' 0=LCD DMD, 1=RealDMD 2=FULLDMD (large/High LCD)
Dim PUPDMDObject  'for realtime mirroring.
DIM PuPGameInfo
DIM PuPGameRunning:PuPGameRunning=false
DIM PuPGameScore
DIM PuPGameTimeout
Dim PuPlayer
Dim quickrestart:quickrestart=0
Dim radarl:radarl=31
Dim radarr:radarr=32
Dim RageCount
Dim RampActivForValidateMission(4)
Dim RampBonus, RampChangeStatus(4), RampLightsBlinkInterval, RampLightsMove(4), RampLightsState
Dim RandomL01(4), RandomL02(4), RandomL03(4), randomLeft
Dim RandomMysterySelect,RandomMysterySelect1, RandomMysterySelect2, RandomMysterySelect3, RandomMysterySelect4
Dim RandomR01(4), RandomR02(4), RandomR03(4), randomRight
Dim randoms:randoms=33
Dim rdown:rdown = 0
Dim RedColorOnly
Dim RemainingHit(4)
Dim ReplayPlayer(4)
Dim RequestANewBall
Dim retimct:retimct=0
Dim RetroMode
Dim reverseLightcounter
Dim reverseLightcounter2
Dim RGBStep, RGBFactor, rRed, rGreen, rBlue, RainbowLights
Dim RGBStep2, RGBFactor2, rRed2, rGreen2, rBlue2, RainbowLights2
dim RightGateCollidable(4)
Dim rrhits(4)
Dim RStep, Lstep
Dim Saves
Dim Score(4)
Dim ScoreBlackCastleChampion(4)
Dim ScoreBonusChampion(4)
Dim ScoreBurningSandsChampion(4)
Dim ScoreCatapultMBChampion(4)
Dim ScoreCent(4), ScorePercent(4), ScoreSternupdated, scorevar(40)
Dim ScoreComboChampion(4)
Dim ScoreDeepFreezeChampion(4)
Dim ScoreKnightChampion(4)
Dim ScoreLoopChampion(4)
Dim ScoreMoltenFireChampion(4)
Dim ScoreMudBogChampion(4)
Dim ScoreSuperChampion(4)
Dim ScoreToAdd
Dim ScoreTripleKnightsMBChampion(4)
Dim ScoreWarChampion(4)
Dim ScoreWickedCavernChampion(4)
Dim screwl:screwl=35
Dim screwr:screwr=36
Dim SelectSuperTargets,SelectSuperSlings,SelectSuperLanes,SelectSuperPops,SelectSuperOrbits,SelectSuperSpinner
Dim seq_animation_array
Dim SeqAnimation, SequenceAnimation
Dim shad_off
Dim Shield_lock
Dim Shield_New_Status			'used to command the shield (lock or unlock)			
Dim ShieldIsReadyToActivateMission(4)
Dim ShieldNumberOfMove			'used for the begining			
Dim ShieldRemainsLock
Dim ShootAgain
dim shotskillflag
Dim skillshotlive:skillshotlive = 0
Dim SkillshotValue
Dim skipped:skipped = 0

Dim songs
	If sndtrk = 1 Then
		songs = "Audiomusic"
	Else
		songs = "Audiomusic"
	End If
Dim SpeakTime
Dim SpecialInfo
Dim spellvo
Dim spinlvl(4)
Dim spinnum(4)
Dim spinvalue
Dim spotstatus:spotstatus = 0
Dim Starmission(4)
Dim StartMission
dim StopBallControlFlag
Dim StopBk2000
Dim StopBouleAngle
Dim StopBouleFlag
Dim StopLightChange
Dim StopRansom
Dim stripe1h:stripe1h=37
Dim stripe1v:stripe1v=38
Dim stripe2h:stripe2h=39
Dim stripe2v:stripe2v=40
Dim subtitle
Dim SuperFeatures, SuperFeaturesColor, SuperFeaturesColorSelected, SuperFeaturesColorNumber, SuperIsLit, SuperModeDelay, SuperOverlaySelected, SuperSelectCalcul
Dim SuperJackpot
Dim SuperTargets,SuperSlings,SuperLanes,SuperPops,SuperOrbits,SuperSpinner
Dim SW41_Flag	'L66    (SW41 + SW45) 	-> Left spinner lane				
Dim SW41Dropped
Dim SW48_Flag	'L72					-> Left orbit
Dim SW51_Flag	'L79					-> Left ramp
Dim SW56_Flag	'L85					-> The Black Knight
Dim SW58_Flag	'L91	(SW58 + SW60)	-> Shield and rear of shield(saucer)			
Dim SW59_Flag	'L98					-> Light Lock target 
Dim SW60Timer
Dim SW60videoDelay
Dim SW64_Flag	'L103					-> Right orbit
Dim SW79_Flag
Dim SWNoHit
Dim TableWidth, TableHeight
Dim TargetBonus
Dim TempoHighScore(5)
Dim TempoHighScoreName(5)
Dim textdefeated
Dim textmission
Dim Tilt
Dim Tilted
Dim TiltSensitivity
Dim tilttime:tilttime = 0
Dim TimeForLastBonus
Dim TimerBeforeBonusScore
Dim title
Dim titlepos
Dim tmpCatapultMBChampion, tmpMoltenFireChampion, tmpDeepFreezeChampion, tmpMudBogChampion, tmpWickedCavernChampion, tmpBurningSandsChampion, tmpSuperChampion
Dim tmpComboChampion, tmpKnightChampion, tmpBlackCastleChampion, tmpLoopChampion, tmpWarChampion, tmpBonusChampion, tmpTripleKnightsMBChampion
Dim tmpScore
Dim TopBallLoop, TotalBonus, totalbumps(4), totalcombo(4), TotalScoreBonus
Dim toppervideo
Dim TotalGamesPlayed
Dim ttable
Dim turnoffrules
Dim turnondmd
Dim udown:udown=17
Dim uleft:uleft=26
Dim UpdateSuperOverlayCount
Dim uright:uright=34
dim useDMDVideos    : useDMDVideos=true   ' true or false to use DMD splash videos.
dim usePuPDMD       : usePuPDMD=true       ' set to false to not use PuPDMD for a DMD (different that BG scoring)
dim useRealDMDScale : useRealDMDScale=0    ' 0 or 1 for RealDMD scaling.  Choose which one you prefer.
Dim Uturn
Dim uup:uup=41
Dim VideoIsPlayed
Dim VideoSelected
Dim WaffleScore(4)
Dim WaffleScoreName(4)
Dim wandjacks(4)
Dim wandlocks(4)
Dim WarCount, WarHurryFlag, WarHurryPhase
Dim WarHurryUp51Touch:Dim WarHurryUp56Touch:Dim WarHurryUp58Touch
Dim WickedCavern(4)
Dim WickedCavernCheckCountRandomLeft(4), WickedCavernCheckCountRandomRight(4), WickedCavernLeftOrRight, WickedCavernLeftOrRightOld
Dim WickedCavernDefeated(4)
Dim wingup:wingup = 0
Dim wiperl:wiperl=42
Dim wiperr:wiperr=43
Dim WithBug:WithBug = False
Dim worldscores
Dim red, orange, amber, yellow, darkgreen, green, blue, darkblue, purple, white, base
	red = 10
	orange = 9
	amber = 8
	yellow = 7
	darkgreen = 6
	green = 5
	blue = 4
	darkblue = 3
	purple = 2
	white = 1
	base = 11				

'*******************************************************************************************************************************************
'*******************************************************************************************************************************************
'*******************************************************************************************************************************************


	
Sub table1_Exit
    Savehs
    If B2SOn = true Then Controller.Stop
End Sub



'********************
'   Light Def
'********************

Dim OldState_L101(4)
Dim OldState_L103(4)
Dim OldState_L106(4)
Dim OldState_L11(4)
Dim OldState_L110(4)
Dim OldState_L111(4)
Dim OldState_L112(4)
Dim OldState_L115(4)
Dim OldState_L116(4)
Dim OldState_L117(4)
Dim OldState_L12(4)
Dim OldState_L13(4)
Dim OldState_L131(4)
Dim OldState_L132(4)
Dim OldState_L133(4)
Dim OldState_L14(4)
Dim OldState_L15(4)
Dim OldState_L16(4)
Dim OldState_L163(4)
Dim OldState_L165(4)
Dim OldState_L166(4)
Dim OldState_L170(4)
Dim OldState_L171(4)
Dim OldState_L172(4)
Dim OldState_L18(4)
Dim OldState_L19(4)
Dim OldState_L20(4)
Dim OldState_L21(4)
Dim OldState_L22(4)
Dim OldState_L23(4)
Dim OldState_L24(4)
Dim OldState_L26(4)
Dim OldState_L29(4)
Dim OldState_L32(4)
Dim OldState_L35(4)
Dim OldState_L38(4)
Dim OldState_L41(4)
Dim OldState_L44(4)
Dim OldState_L47(4)
Dim OldState_L48(4)
Dim OldState_L49(4)
Dim OldState_L50(4)
Dim OldState_L53(4)
Dim OldState_L56(4)
Dim OldState_L59(4)
Dim OldState_L63(4)
Dim OldState_L64(4)
Dim OldState_L66(4)
Dim OldState_L69(4)
Dim OldState_L70(4)
Dim OldState_L72(4)
Dim OldState_L75(4)
Dim OldState_L79(4)
Dim OldState_L82(4)
Dim OldState_L83(4)
Dim OldState_L85(4)
Dim OldState_L88(4)
Dim OldState_L89(4)
Dim OldState_L91(4)
Dim OldState_L94(4)
Dim OldState_L95(4)
Dim OldState_L96(4)
Dim OldState_L98(4)
Dim OldState_Light026(4)
Dim OldState_Light054(4)
Dim OldState_Light055(4)
Dim OldState_Light056(4)
Dim OldState_Light057(4)
Dim OldState_Light058(4)
Dim OldState_Light059(4)
Dim OldState_Light060(4)
Dim OldState_Light061(4)
Dim OldState_Light062(4)
Dim OldState_Light063(4)




EnableRetractPlunger = false 'Change to true to enable retracting the plunger at a linear speed; wait for 1 second at the maximum position; move back towards the resting position; nice for button/key plungers

'**********************************************************************************************************************************
'*************************************************************************************
'************************************DOF section**************************************
'*************************************************************************************
Sub video_init()
	BypassVideo = False
	PleaseWait = False
	'DOF 1, DOFPulse
	HideOverlay = True
'	DOF 499, DOFPulse
	pupevent 499
'	BackGroundFileIs = "BlackKnight_PhotoShoot_TitleScreen.mp4"
'	BackGroundFolderIs = "Background looping"
'	BackGroundLenghtIs = 3000
'''	SetBackGroundGeneral
'	puPlayer.LabelSet pBackglass,"CommentDisplayed","Mudbog Total",1,""
''	puPlayer.LabelSet pBackglass,"CommentDisplayed2","Mudbog Total",1,""
'	puPlayer.LabelSet pBackglass,"AddScoreDisplayed","1,000,000",1,""
'	puPlayer.LabelSet pBackglass,"AddScoreDisplayed2","1,000,000",1,""
''	pSplashScore "Mudbog Total",4.1,255
'	pSplashScore "Mudbog Total",4,40

'	pSplashCommentDisplayed "7777777777777",10,255
'	pSplashCommentDisplayed2 "7777777777777",10,40
'	pSplashAddScoreDisplayed "88888888888888",10,255
'	pSplashAddScoreDisplayed2 "88888888888888",10,40

'	puPlayer.LabelSet pBackglass,"LastChanceTime","30 ",1,""

End Sub

Sub video_init2()
	BypassVideo = False
	PleaseWait = False
	'DOF 1, DOFPulse
	HideOverlay = True
'	DOF 499, DOFPulse
	pupevent 499
'''	SetBackGroundImportant.Enabled = True
'	VariableTemporaire = VariableTemporaire +1
'	AttractMode.Enabled = True
'	BackGroundFileIs = "BlackKnight_PhotoShoot_TitleScreen.mp4"
'	BackGroundFolderIs = "Background looping"
'	BackGroundLenghtIs = 3000
'''	SetBackGroundGeneral
End Sub

Sub AttractMode_Timer()
	BypassVideo = False
	AttractMode.Enabled = False
	pAttractNextBackglass
End Sub

Sub pAttractNextBackglass
	BypassVideo = False
	pCurAttractPos=pCurAttractPos+1
	If bAttractMode = True Then
		Select Case pCurAttractPos
			Case 1 PuPlayer.playlistplayex pBackglass,"Attract loop","ATTRACT_LOOP1.mp4",0,1: AttractMode.Enabled = True : AttractMode.interval=17000
			Case 2 PuPlayer.playlistplayex pBackglass,"Attract loop","ATTRACT_LOOP2.mp4",0,1: AttractMode.Enabled = True : AttractMode.interval=17000
			Case 3 PuPlayer.playlistplayex pBackglass,"Attract loop","ATTRACT_LOOP3.mp4",0,1: AttractMode.Enabled = True : AttractMode.interval=17000
			Case 4 PuPlayer.playlistplayex pBackglass,"Tutorial Premium","Black Knight Premium - Catapult Multiball.mp4",0,1: AttractMode.Enabled = True : AttractMode.interval=36000
			Case 5 PuPlayer.playlistplayex pBackglass,"Tutorial Premium","Black Knight Premium - Lightning Wheel Modes.mp4",0,1: AttractMode.Enabled = True : AttractMode.interval=47000
			Case 6 PuPlayer.playlistplayex pBackglass,"Tutorial Premium","Black Knight Premium - Triple Knights Challenge Multiball.mp4",0,1: AttractMode.Enabled = True : AttractMode.interval=44000
			Case 7 PuPlayer.playlistplayex pBackglass,"Tutorial Premium","Black Knight Premium - Triple Knights Challenge Multiball.mp4",0,1: AttractMode.Enabled = True : AttractMode.interval=56000
		Case Else
			pCurAttractPos=0
			pAttractNextBackglass 'reset to beginning
		end Select
	Else
		AttractMode.Enabled = False
	End If
End Sub


Sub video_fire_animation()
	BypassVideo = False
	EndOfVideo.Interval=6000
	EndOfVideo.Enabled=True
	If WarHurryFlag = False Then
		VideoSelected = "SkeletonPops_BGLoop.mp4"
'		pupevent 584
	Else
		VideoSelected = "SkeletonPopsBK_BGLoop.mp4"
'		pupevent 585
	End If
	playmedia VideoSelected,"Skeleton BG",pBackglass,"cineon",6000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
End Sub


Sub Video_WarHurryUp()
	BypassVideo = False
	HideOverlay = False
	restoreOverlay.Interval=100
	restoreOverlay.Enabled=True
'	DOF 585, DOFPulse
	pupevent 585
'	BackGroundFileIs = "SkeletonPopsBK_BGLoop.mp4"
'	BackGroundFolderIs = "Skeleton BG"
'	BackGroundLenghtIs = 15000
'''	SetBackGroundGeneral	
End Sub

Sub Videos_Ball_Saved()   'PuP - Ball Saved
	BypassVideo = False
	HideOverlay = True
	VideoHideOverlay
	EndOfVideo.Interval=6000
	EndOfVideo.Enabled=True
	'DOF 58, DOFPulse
	playmedia "BallSave.mp4","Ball Save",pBackglass,"cineon",5000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	pupevent 506
End Sub
Sub video_catapult_lock_is_lit()
	BypassVideo = True
	HideOverlay = True
	VideoHideOverlay
	EndOfVideo.Interval=3000
	EndOfVideo.Enabled=True
	playmedia "BlackKnight_CatapultMultiball_LockIsLit.mp4","catapult ball is lit",pBackglass,"cineon",3000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	pupevent 516
End Sub


'**********************************************************************************************************************************************************************
'*					Bumper Animation
'**********************************************************************************************************************************************************************

Sub BumpSW70animationHit_timer() 						'*********** Hit ************* SW70
	If compteuriSW70Hit = 721 Then
		compteuriSW70Hit = 701
		BumpSW70animationHit.Enabled = False
	Else
		If WithBug = True Then
'			DOF compteuriSW70Hit, DOFPulse
			pupevent compteuriSW70Hit
		End If
	End If
	compteuriSW70Hit = compteuriSW70Hit + 1
End Sub
Sub BumpSW70AnimationIdle_timer()  						'*********** Idle ************* SW70
	If compteuriSW70Idle = 738 Then
		compteuriSW70Idle = 722
		BumpSW70AnimationIdle.Enabled = False
	Else
		If WithBug = True Then
'			DOF compteuriSW70Idle, DOFPulse
			pupevent compteuriSW70Idle
		End If
	End If
	compteuriSW70Idle = compteuriSW70Idle + 1
End Sub
Sub BumpSW70AnimationDeath_timer()  					'*********** Death ************* SW70
	If compteuriSW70Death = 756 Then
		compteuriSW70Death = 739
		BumpSW70AnimationDeath.Enabled = False
	Else
		If WithBug = True Then
'			DOF compteuriSW70Death, DOFPulse
			pupevent compteuriSW70Death
		End If
	End If
	compteuriSW70Death = compteuriSW70Death + 1
End Sub




Sub BumpSW71animationHit_timer() 						'*********** Hit ************* SW71
	If compteuriSW71Hit = 778 Then
		compteuriSW71Hit = 757
		BumpSW71animationHit.Enabled = False
	Else
		If WithBug = True Then
'			DOF compteuriSW71Hit, DOFPulse
			pupevent compteuriSW71Hit
		End If
	End If
	compteuriSW71Hit = compteuriSW71Hit + 1
End Sub
Sub BumpSW71AnimationIdle_timer()  						'*********** Idle ************* SW71
	If compteuriSW71Idle = 795 Then
		compteuriSW71Idle = 779
		BumpSW71AnimationIdle.Enabled = False
	Else
		If WithBug = True Then
'			DOF compteuriSW71Idle, DOFPulse
			pupevent compteuriSW71Idle
		End If
	End If
	compteuriSW71Idle = compteuriSW71Idle + 1
End Sub
Sub BumpSW71AnimationDeath_timer()  					'*********** Death ************* SW71
	If compteuriSW71Death = 813 Then
		compteuriSW71Death = 796
		BumpSW71AnimationDeath.Enabled = False
	Else
		If WithBug = True Then
'			DOF compteuriSW71Death, DOFPulse
			pupevent compteuriSW71Death
		End If
	End If
	compteuriSW71Death = compteuriSW71Death + 1
End Sub




Sub BumpSW72animationHit_timer() 						'*********** Hit ************* SW72
	If compteuriSW72Hit = 842 Then
		compteuriSW72Hit = 814
		BumpSW72animationHit.Enabled = False
	Else
		If WithBug = True Then
'			DOF compteuriSW72Hit, DOFPulse
			pupevent compteuriSW72Hit
		End If
	End If
	compteuriSW72Hit = compteuriSW72Hit + 1
End Sub
Sub BumpSW72AnimationIdle_timer()  						'*********** Idle ************* SW72
	If compteuriSW72Idle = 859 Then
		compteuriSW72Idle = 843
		BumpSW72AnimationIdle.Enabled = False
	Else
		If WithBug = True Then
'			DOF compteuriSW72Idle, DOFPulse
			pupevent compteuriSW72Idle
		End If
	End If
	compteuriSW72Idle = compteuriSW72Idle + 1
End Sub
Sub BumpSW72AnimationDeath_timer()  					'*********** Death ************* SW72
	If compteuriSW72Death = 881 Then
		compteuriSW72Death = 860
		BumpSW72AnimationDeath.Enabled = False
	Else
		If WithBug = True Then
'			DOF compteuriSW72Death, DOFPulse
			pupevent compteuriSW72Death
		End If
	End If
	compteuriSW72Death = compteuriSW72Death + 1
End Sub



'*********************************************
'** Number corresponding to puppack editor ***
'*********************************************
Sub AnimBumpSW70Hit()							'************************************* SW70 ******************************************
	video_fire_animation
	compteuriSW70Hit = 701
	BumpSW70animationHit.Enabled = True
End Sub
Sub AnimBumpSW70Idle()
	video_fire_animation
	compteuriSW70Idle = 722
	BumpSW70animationIdle.Enabled = True
End Sub
Sub AnimBumpSW70Death()
	video_fire_animation
	compteuriSW70Death = 739
	BumpSW70animationDeath.Enabled = True
End Sub
Sub AnimBumpSW71Hit()							'************************************* SW71 ******************************************
	video_fire_animation
	compteuriSW71Hit = 757
	BumpSW71animationHit.Enabled = True
End Sub
Sub AnimBumpSW71Idle()
	video_fire_animation
	compteuriSW71Idle = 779
	BumpSW71animationIdle.Enabled = True
End Sub
Sub AnimBumpSW71Death()
	video_fire_animation
	compteuriSW71Death = 796
	BumpSW71animationDeath.Enabled = True
End Sub
Sub AnimBumpSW72Hit()							'************************************* SW72 ******************************************
	video_fire_animation
	compteuriSW72Hit = 814
	BumpSW72animationHit.Enabled = True
End Sub
Sub AnimBumpSW72Idle()
	video_fire_animation
	compteuriSW72Idle = 843
	BumpSW72animationIdle.Enabled = True
End Sub
Sub AnimBumpSW72Death()
	video_fire_animation
	compteuriSW72Death = 860
	BumpSW72animationDeath.Enabled = True
End Sub

'*****************************************************************************************************************************************************


Sub Fireanimation_timer() ' Not Used because PinUpPlayer Crash
	BypassVideo = False
	If compteuri = 1120 Then
		compteuri = 1000
		Fireanimation.Enabled = False
		DOF 900, DOFPulse 
	Elseif compteuri > 1120 Then
		compteuri = 1119
	Else
		Filenamepng = compteuri&".png"
		playmedia Filenamepng,"Fire animation",pBackGlass2,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	End If
	compteuri = compteuri + 1
End Sub


Sub Video_FireAnimation2
	BypassVideo = False
'	HideOverlay = True
'	VideoHideOverlay
	EndOfVideo.Interval=6000
	EndOfVideo.Enabled=True
	Endvideo_fire.Interval=6000
	Endvideo_fire.Enabled=True
	FireAddScore.Enabled=True
'	playmedia "BK Fire integrated.mp4","Fire Animation2",pBackglass,"cineon",6000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)

'	compteuri = 901
	compteuri = 1001

	If WithBug = True Then
		Fireanimation.Enabled = True
	Else
		If OrbitFlag = True Then
			EndOfVideo.Interval=5000
			playmedia "BK Fire integrated 01.mp4","Fire Animation2",pBackglass,"cineon",5000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		Else
			EndOfVideo.Interval=9500
			playmedia "BK Fire integrated 02.mp4","Fire Animation2",pBackglass,"cineon",9500,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		End If
		EndOfVideo.Enabled=True
	End If
	playmedia "Sound-0x0802.mp3","Audio Monster 1",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
End Sub

Sub VideocatapultBall1lock
	DOF 601, DOFPulse
	BypassVideo = True
	HideOverlay = True
	VideoHideOverlay
	EndOfVideo.Interval=3000
	EndOfVideo.Enabled=True
	playmedia "BlackKnight_CatapultMultiball_Ball_1_Lock.mp4","Catapult lock",pBackglass,"cineon",2500,"",1,6  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	pupevent 517
End Sub
Sub VideocatapultBall2lock
	DOF 602, DOFPulse
	BypassVideo = True
	HideOverlay = True
	VideoHideOverlay
	EndOfVideo.Interval=3000
	EndOfVideo.Enabled=True
	playmedia "BlackKnight_CatapultMultiball_Ball_2_Lock.mp4","Catapult lock",pBackglass,"cineon",2500,"",1,6  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	pupevent 518
End Sub
Sub VideocatapultBall3lock
	DOF 603, DOFPulse
	DOF 604, DOFOn
	BypassVideo = True
	HideOverlay = True
	VideoHideOverlay
'	EndOfVideo.Interval=5100
'	EndOfVideo.Enabled=True
	playmedia "BlackKnight_CatapultMultiball_Ball_3_Lock_ModeStart.mp4","Catapult lock",pBackglass,"cineon",6500,"",1,6  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	pupevent 519
	VideoCatapultLoopSetBackground.Interval=6500
	VideoCatapultLoopSetBackground.Enabled = True
	L131.state=0
	L132.state=0
	L133.state=0
End Sub
Sub VideoSupercatapultBall1lock
				  
	BypassVideo = False
	HideOverlay = True
	VideoHideOverlay
	EndOfVideo.Interval=3000
	EndOfVideo.Enabled=True
	playmedia "BlackKnight_SuperCatapultMultiball_Ball_1_Lock.mp4","Catapult lock",pBackglass,"cineon",2500,"",1,6  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	pupevent 517
End Sub
Sub VideoSupercatapultBall2lock
				  
	BypassVideo = False
	HideOverlay = True
	VideoHideOverlay
	EndOfVideo.Interval=3000
	EndOfVideo.Enabled=True
	playmedia "BlackKnight_SuperCatapultMultiball_Ball_2_Lock.mp4","Catapult lock",pBackglass,"cineon",2500,"",1,6  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	pupevent 518
End Sub
Sub VideoSupercatapultBall3lock
				  
			   
	BypassVideo = False
	HideOverlay = True
	VideoHideOverlay
'	EndOfVideo.Interval=5100
'	EndOfVideo.Enabled=True
	playmedia "BlackKnight_SuperCatapultMultiball_Ball_3_Lock_ModeStart.mp4","Catapult lock",pBackglass,"cineon",6900,"",1,6  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	pupevent 519
'	VideoCatapultLoopSetBackground.Interval=6900
'	VideoCatapultLoopSetBackground.Enabled = True
	L131.state=0
	L132.state=0
	L133.state=0
End Sub
Sub videoshotput
	BypassVideo = False
	HideOverlay = True
	VideoHideOverlay
'	EndOfVideo.Interval=1000
	EndOfVideo.Enabled=True
	Select Case Int(Rnd*4)+1
		Case 1 : VideoSelected = "BlackKnight_MacenLoops_Spin1.mp4" : EndOfVideo.Interval=2000
		Case 2 : VideoSelected = "BlackKnight_MacenLoops_Spin2.mp4" : EndOfVideo.Interval=2000
		Case 3 : VideoSelected = "BlackKnight_MacenLoops_Spin3.mp4" : EndOfVideo.Interval=2000
		Case 4 : VideoSelected = "BlackKnight_MacenLoops_Toss.mp4" : EndOfVideo.Interval=5000
	End Select
	playmedia VideoSelected,"shotput",pBackglass,"cineon",1000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	pupevent 583
End Sub
Sub videoshotskill
	BypassVideo = True
	HideOverlay = True
	VideoHideOverlay
	EndOfVideo.Interval=5500
	EndOfVideo.Enabled=True
	DOF 600, DOFPulse
	Select Case Int(Rnd*2)+1
		Case 1 : VideoSelected = "BlackKnight_SkillShot_1.mp4"
		Case 2 : VideoSelected = "BlackKnight_SkillShot_2.mp4"
	End Select
	playmedia VideoSelected,"Super Skill shot",pBackglass,"cineon",4750,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	pupevent 587
End Sub
Sub VideoReplay
	BypassVideo = False
	HideOverlay = True
	EndOfVideo.Interval=6400
	EndOfVideo.Enabled=True
	VideoHideOverlay
	playmedia "Replay.mp4","Match",pBackglass,"cineon",6000,"",1,35  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
End Sub

Sub VideoNewRecord
	BypassVideo = False
	HideOverlay = True
	EndOfVideo.Interval=4000
	EndOfVideo.Enabled=True
	VideoHideOverlay
	pSplashCommentDisplayed "Player "&PlayerCheckCurrent,2,0
	pSplashCommentDisplayed2 "Player "&PlayerCheckCurrent,2,30719
	pSplashAddScoreDisplayed "enter initials",2,0
	pSplashAddScoreDisplayed2 "enter initials",2,30719
	playmedia "HightScore.mp4","Match",pBackglass,"cineon",4000,"",1,35  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
End Sub

Sub VideoFireElementIntro
	BypassVideo = True
	HideOverlay = True
	EndOfVideo.Interval=6400
	EndOfVideo.Enabled=True
	VideoHideOverlay
	playmedia "BlackKnight_FireElementalBattle_ModeStart.mp4","Fire Elemental Mode",pBackglass,"cineon",6000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	pupevent 532
End Sub
Sub VideoHandHolderIntro
	BypassVideo = True
	HideOverlay = True
	EndOfVideo.Interval=8000
	EndOfVideo.Enabled=True
	VideoHideOverlay
	playmedia "BK_WheelMode_Handholder_Intro.mp4","Hand holder Mode",pBackglass,"cineon",8000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	pupevent 536
End Sub
Sub VideoHydraIntro
	BypassVideo = True
	HideOverlay = True
	EndOfVideo.Interval=8000
	EndOfVideo.Enabled=True
	VideoHideOverlay
	playmedia "BlackKnight_HydraBattle_Intro.mp4","Hydra Mode",pBackglass,"cineon",7000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	pupevent 540
End Sub
Sub VideoLichIntro
	BypassVideo = True
	HideOverlay = True
	EndOfVideo.Interval=8000
	EndOfVideo.Enabled=True
	VideoHideOverlay
	playmedia "BK_WheelMode_Lich_Intro.mp4","Lich Mode",pBackglass,"cineon",7000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
End Sub
Sub VideoSandwormIntro
	BypassVideo = True
	HideOverlay = True
	EndOfVideo.Interval=5000
	EndOfVideo.Enabled=True
	VideoHideOverlay
	playmedia "BK_WheelMode_Sandworm_Intro.mp4","Sandworm Mode",pBackglass,"cineon",5000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	pupevent 579
End Sub

Sub VideoLastChanceIntro_Timer()
	BypassVideo = False
	VideoLastChanceIntro.Enabled = False
	HideOverlay = True
	EndOfVideoLastChance.Interval=5000
	EndOfVideoLastChance.Enabled=True
	VideoHideOverlay
	playmedia "LastChance.mp4","Last Chance",pBackglass,"cineon",5000,"",1,35  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	priority = 35
End Sub

Sub VideoFireElementLoop
	BypassVideo = False
	HideOverlay = False
	restoreOverlay.Interval=100
	restoreOverlay.Enabled=True
				   
	If vsvol = 100 Then 
'		DOF 533, DOFPulse
		pupevent 533
	ElseIf vsvol < 100 And vsvol > 89 Then pupevent 929
	ElseIf vsvol < 90 And vsvol > 79  Then pupevent 928
	ElseIf vsvol < 80 And vsvol > 69  Then pupevent 927
	ElseIf vsvol < 70 And vsvol > 59  Then pupevent 926
	ElseIf vsvol < 60 And vsvol > 49  Then pupevent 925
	ElseIf vsvol < 50 And vsvol > 39  Then pupevent 924
	ElseIf vsvol < 40 And vsvol > 29  Then pupevent 923
	ElseIf vsvol < 30 And vsvol > 19  Then pupevent 922
	Else pupevent 921
	End If
'	SetBackGroundImportant.Interval = 15000
'	SetBackGroundImportant.Enabled = True

'	BackGroundFileIs = "BlackKnight_FireElementalBattle_LoopingMainplayBackground.mp4"
'	BackGroundFolderIs = "Fire Elemental Mode"
'	BackGroundLenghtIs = 15000
'''	SetBackGroundGeneral

'	playmedia "BlackKnight_FireElementalBattle_LoopingMainplayBackground.mp4","Fire Elemental Mode",pBackglass,"cineon",15000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	PuPlayer.setbackground pBackglass, 1  


'	PuPlayer.SetLoop 2,1
'	pupevent 533

'''	SetBackGroundGeneral
End Sub
Sub VideoHandHolderLoop
	BypassVideo = False
	HideOverlay = False
	restoreOverlay.Interval=100
	restoreOverlay.Enabled=True
				   
	If vsvol = 100 Then 
'		DOF 537, DOFPulse
		pupevent 537
	ElseIf vsvol < 100 And vsvol > 89 Then pupevent 939
	ElseIf vsvol < 90 And vsvol > 79  Then pupevent 938
	ElseIf vsvol < 80 And vsvol > 69  Then pupevent 937
	ElseIf vsvol < 70 And vsvol > 59  Then pupevent 936
	ElseIf vsvol < 60 And vsvol > 49  Then pupevent 935
	ElseIf vsvol < 50 And vsvol > 39  Then pupevent 934
	ElseIf vsvol < 40 And vsvol > 29  Then pupevent 933
	ElseIf vsvol < 30 And vsvol > 19  Then pupevent 932
	Else pupevent 931
	End If
'	SetBackGroundImportant.Interval = 20000
'	SetBackGroundImportant.Enabled = True

'	BackGroundFileIs = "BK_WheelMode_Handholder_BGIdle.mp4"
'	BackGroundFolderIs = "Hand holder Mode"
'	BackGroundLenghtIs = 20000
'''	SetBackGroundGeneral

'	playmedia "BK_WheelMode_Handholder_BGIdle.mp4","Hand holder Mode",pBackglass,"cineon",20000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	PuPlayer.setbackground pBackglass, 1  
'	PuPlayer.SetLoop 2,1 
'	pupevent 537
End Sub
Sub VideoHydraLoop
	BypassVideo = False
	HideOverlay = False
	restoreOverlay.Interval=100
	restoreOverlay.Enabled=True
				   
	If vsvol = 100 Then 
		'DOF 541, DOFPulse
		pupevent 541
	ElseIf vsvol < 100 And vsvol > 89 Then pupevent 949
	ElseIf vsvol < 90 And vsvol > 79  Then pupevent 948
	ElseIf vsvol < 80 And vsvol > 69  Then pupevent 947
	ElseIf vsvol < 70 And vsvol > 59  Then pupevent 946
	ElseIf vsvol < 60 And vsvol > 49  Then pupevent 945
	ElseIf vsvol < 50 And vsvol > 39  Then pupevent 944
	ElseIf vsvol < 40 And vsvol > 29  Then pupevent 943
	ElseIf vsvol < 30 And vsvol > 19  Then pupevent 942
	Else pupevent 941
	End If
'	SetBackGroundImportant.Interval = 16000
'	SetBackGroundImportant.Enabled = True

'	BackGroundFileIs = "BlackKnight_HydraBattle_BGIdle.mp4"
'	BackGroundFolderIs = "Hydra Mode"
'	BackGroundLenghtIs = 16000
'''	SetBackGroundGeneral

'	playmedia "BlackKnight_HydraBattle_BGIdle.mp4","Hydra Mode",pBackglass,"cineon",16000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	PuPlayer.setbackground pBackglass, 1  
'	PuPlayer.SetLoop 2,1 
'	pupevent 541
End Sub
Sub VideoLichLoop
	BypassVideo = False
	HideOverlay = False
	restoreOverlay.Interval=100
	restoreOverlay.Enabled=True
				   
	If vsvol = 100 Then 
	'	DOF 571, DOFPulse
		pupevent 571
	ElseIf vsvol < 100 And vsvol > 89 Then pupevent 959
	ElseIf vsvol < 90 And vsvol > 79  Then pupevent 958
	ElseIf vsvol < 80 And vsvol > 69  Then pupevent 957
	ElseIf vsvol < 70 And vsvol > 59  Then pupevent 956
	ElseIf vsvol < 60 And vsvol > 49  Then pupevent 955
	ElseIf vsvol < 50 And vsvol > 39  Then pupevent 954
	ElseIf vsvol < 40 And vsvol > 29  Then pupevent 953
	ElseIf vsvol < 30 And vsvol > 19  Then pupevent 952
	Else pupevent 951
	End If
'	SetBackGroundImportant.Interval = 20000
'	SetBackGroundImportant.Enabled = True

'	BackGroundFileIs = "BK_WheelMode_Lich_BGIdle.mp4"
'	BackGroundFolderIs = "Lich Mode"
'	BackGroundLenghtIs = 20000
'''	SetBackGroundGeneral

'	playmedia "BK_WheelMode_Lich_BGIdle.mp4","Lich Mode",pBackglass,"cineon",20000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	PuPlayer.setbackground pBackglass, 1  
	'PuPlayer.SetLoop 2,1
'	pupevent 571
End Sub
Sub VideoSandwormLoop
	BypassVideo = False
	HideOverlay = False
	restoreOverlay.Interval=100
	restoreOverlay.Enabled=True
				   
	If vsvol = 100 Then 
'		DOF 580, DOFPulse
		pupevent 580
	ElseIf vsvol < 100 And vsvol > 89 Then pupevent 969
	ElseIf vsvol < 90 And vsvol > 79  Then pupevent 968
	ElseIf vsvol < 80 And vsvol > 69  Then pupevent 967
	ElseIf vsvol < 70 And vsvol > 59  Then pupevent 966
	ElseIf vsvol < 60 And vsvol > 49  Then pupevent 965
	ElseIf vsvol < 50 And vsvol > 39  Then pupevent 964
	ElseIf vsvol < 40 And vsvol > 29  Then pupevent 963
	ElseIf vsvol < 30 And vsvol > 19  Then pupevent 962
	Else pupevent 961
	End If
'	SetBackGroundImportant.Interval = 18000
'	SetBackGroundImportant.Enabled = True

'	BackGroundFileIs = "BK_WheelMode_SandWorm_BGIdle.mp4"
'	BackGroundFolderIs = "Sandworm Mode"
'	BackGroundLenghtIs = 18000
'''	SetBackGroundGeneral

'	playmedia "BK_WheelMode_SandWorm_BGIdle.mp4","Sandworm Mode",pBackglass,"cineon",18000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	PuPlayer.setbackground pBackglass, 1  
	'PuPlayer.SetLoop 2,1 
'	pupevent 580
End Sub

Sub ModeTotalHydra_Timer()
	BypassVideo = False
	CurrentMissionFlag(CurrentPlayer) = 0
	VideoIsPlayed = True
	ModeTotalHydra.Enabled = False
	HideOverlay = True
	VideoHideOverlay
	EndOfMode.Interval=4000
	EndOfMode.Enabled=True
	playmedia "BlackKnight_HydraBattle_ModeTotal.mp4","Hydra Mode",pBackglass,"cineon",3000,"",1,20  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	puPlayer.LabelSet pBackglass,"CommentDisplayed","Mudbog Total",1,""
'	puPlayer.LabelSet pBackglass,"CommentDisplayed2","Mudbog Total",1,""
'	puPlayer.LabelSet pBackglass,"AddScoreDisplayed","" & FormatNumber(AddScoreMudBogTotal(CurrentPlayer),0),1,""
'	puPlayer.LabelSet pBackglass,"AddScoreDisplayed2","" & FormatNumber(AddScoreMudBogTotal(CurrentPlayer),0),1,""
	If priority <= 20 Then
		pSplashCommentDisplayed "Mudbog Total",3,255
		pSplashCommentDisplayed2 "Mudbog Total",3,40
		pSplashAddScoreDisplayed "" & FormatNumber(AddScoreMudBogTotal(CurrentPlayer),0),3,255
		pSplashAddScoreDisplayed2 "" & FormatNumber(AddScoreMudBogTotal(CurrentPlayer),0),3,40
	End If
'	pupevent 542
	ScoreMudBogChampion(CurrentPlayer) = ScoreMudBogChampion(CurrentPlayer) + AddScoreMudBogTotal(CurrentPlayer)
	AddScoreMudBogTotal(CurrentPlayer) = 0
End Sub

Sub ModeTotalFireElement_Timer()
	BypassVideo = False
	CurrentMissionFlag(CurrentPlayer) = 0
	VideoIsPlayed = True
	ModeTotalFireElement.Enabled = False
	HideOverlay = True
	VideoHideOverlay
	EndOfMode.Interval=8000
	EndOfMode.Enabled=True
	playmedia "BlackKnight_FireElementalBattle_ModeTotal.mp4","Fire Elemental Mode",pBackglass,"cineon",3000,"",1,20  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	If priority <= 20 Then
		pSplashCommentDisplayed "Molten Fire Total",3,255
		pSplashCommentDisplayed2 "Molten Fire Total",3,40
		pSplashAddScoreDisplayed "" & FormatNumber(AddScoreMoltenFireTotal(CurrentPlayer),0),3,255
		pSplashAddScoreDisplayed2 "" & FormatNumber(AddScoreMoltenFireTotal(CurrentPlayer),0),3,40
	End If
'	pupevent 534
	ScoreMoltenFireChampion(CurrentPlayer) = ScoreMoltenFireChampion(CurrentPlayer) + AddScoreMoltenFireTotal(CurrentPlayer)
	AddScoreMoltenFireTotal(CurrentPlayer) = 0
End Sub

Sub ModeTotalSandWorm_Timer()
	BypassVideo = False
	CurrentMissionFlag(CurrentPlayer) = 0
	VideoIsPlayed = True
	ModeTotalSandWorm.Enabled = False
	HideOverlay = True
	VideoHideOverlay
	EndOfMode.Interval=1900
	EndOfMode.Enabled=True
	playmedia "BK_WheelMode_Sandworm_ModeTotal.mp4","Sandworm Mode",pBackglass,"cineon",3000,"",1,20  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	If priority <= 20 Then
		pSplashCommentDisplayed "Burning Sands Total",3,255
		pSplashCommentDisplayed2 "Burning Sands Total",3,40
		pSplashAddScoreDisplayed "" & FormatNumber(AddScoreSandWormTotal(CurrentPlayer),0),3,255
		pSplashAddScoreDisplayed2 "" & FormatNumber(AddScoreSandWormTotal(CurrentPlayer),0),3,40
	End If
'	pupevent 581
	ScoreBurningSandsChampion(CurrentPlayer) = ScoreBurningSandsChampion(CurrentPlayer) + AddScoreSandWormTotal(CurrentPlayer)
	AddScoreSandWormTotal(CurrentPlayer) = 0
End Sub

Sub ModeTotalHandHolder_Timer()
	BypassVideo = False
	CurrentMissionFlag(CurrentPlayer) = 0
	VideoIsPlayed = True
	ModeTotalHandHolder.Enabled = False
	HideOverlay = True
	VideoHideOverlay
	EndOfMode.Interval=2650
	EndOfMode.Enabled=True
	playmedia "BK_WheelMode_Handholder_ModeTotal.mp4","Hand holder Mode",pBackglass,"cineon",2850,"",1,20  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	If priority <= 20 Then
		pSplashCommentDisplayed "Wicked Cavern Total",3,255
		pSplashCommentDisplayed2 "Wicked Cavern Total",3,40
		pSplashAddScoreDisplayed "" & FormatNumber(AddScoreWickedCavernTotal(CurrentPlayer),0),3,255
		pSplashAddScoreDisplayed2 "" & FormatNumber(AddScoreWickedCavernTotal(CurrentPlayer),0),3,40
	End If
'	pupevent 538
	ScoreWickedCavernChampion(CurrentPlayer) = ScoreWickedCavernChampion(CurrentPlayer) + AddScoreWickedCavernTotal(CurrentPlayer)
	AddScoreWickedCavernTotal(CurrentPlayer) = 0
End Sub

Sub ModeTotalLich_Timer()
	BypassVideo = False
	CurrentMissionFlag(CurrentPlayer) = 0
	VideoIsPlayed = True
	ModeTotalLich.Enabled = False
	HideOverlay = True
	VideoHideOverlay
	EndOfMode.Interval=4050
	EndOfMode.Enabled=True
	playmedia "BlackKnight_LichBattle_ModeComplete.mp4","Lich Mode",pBackglass,"cineon",3000,"",1,20  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	If priority <= 20 Then
		pSplashCommentDisplayed "Deep Freeze Total",3,255
		pSplashCommentDisplayed2 "Deep Freeze Total",3,40
		pSplashAddScoreDisplayed "" & FormatNumber(AddScoreDeepFreezeTotal(CurrentPlayer),0),3,255
		pSplashAddScoreDisplayed2 "" & FormatNumber(AddScoreDeepFreezeTotal(CurrentPlayer),0),3,40
	End If
'	pupevent 572
	ScoreDeepFreezeChampion(CurrentPlayer) = ScoreDeepFreezeChampion(CurrentPlayer) + AddScoreDeepFreezeTotal(CurrentPlayer)
	AddScoreDeepFreezeTotal(CurrentPlayer) = 0
End Sub

Sub ModeTotalBlackCastle_Timer()
	BypassVideo = False
	CurrentMissionFlag(CurrentPlayer) = 0
	VideoIsPlayed = True
	ModeTotalBlackCastle.Enabled = False
	HideOverlay = True
	VideoHideOverlay
	EndOfMode.Interval=4000
	EndOfMode.Enabled=True
	playmedia "BlackKnight_BKBattle_ModeTotal.mp4","Black Castle Mode",pBackglass,"cineon",3000,"",1,20  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	If priority <= 20 Then
		pSplashCommentDisplayed "Black Castle Total",3,255
		pSplashCommentDisplayed2 "Black Castle Total",3,40
		pSplashAddScoreDisplayed "" & FormatNumber(AddScoreBlackCastleTotal(CurrentPlayer),0),3,255
		pSplashAddScoreDisplayed2 "" & FormatNumber(AddScoreBlackCastleTotal(CurrentPlayer),0),3,40
	End If
'	pupevent 514
	ScoreBlackCastleChampion(CurrentPlayer) = ScoreBlackCastleChampion(CurrentPlayer) + AddScoreBlackCastleTotal(CurrentPlayer)
	AddScoreBlackCastleTotal(CurrentPlayer) = 0
End Sub

Sub ModeTotalCatapult_Timer()
	BypassVideo = False
	VideoIsPlayed = True
	ModeTotalCatapult.Enabled = False
	HideOverlay = True
	VideoHideOverlay
	EndOfMode.Interval=4750
	EndOfMode.Enabled=True
	playmedia "BlackKnight_CatapultMultiball_ModeTotal.mp4","Catapult Multiball",pBackglass,"cineon",4750,"",1,20  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	puPlayer.LabelSet pBackglass,"CommentDisplayed","Catapult Multiball Total",1,""
'	puPlayer.LabelSet pBackglass,"CommentDisplayed2","Catapult Multiball Total",1,""
'	puPlayer.LabelSet pBackglass,"AddScoreDisplayed","" & FormatNumber(Catapult_Mode_Total_score,0),1,""
'	puPlayer.LabelSet pBackglass,"AddScoreDisplayed2","" & FormatNumber(Catapult_Mode_Total_score,0),1,""
	If priority <= 20 Then
		pSplashCommentDisplayed "Catapult Multiball Total",3,255
		pSplashCommentDisplayed2 "Catapult Multiball Total",3,40
		pSplashAddScoreDisplayed "" & FormatNumber(Catapult_Mode_Total_score(CurrentPlayer),0),3,255
		pSplashAddScoreDisplayed2 "" & FormatNumber(Catapult_Mode_Total_score(CurrentPlayer),0),3,40
	End If
	AddscoreCatapultBonus = 400000
	LastChanceCatapultMode = False 
'	pupevent 523
	ScoreCatapultMBChampion(CurrentPlayer) = ScoreCatapultMBChampion(CurrentPlayer) + Catapult_Mode_Total_score(CurrentPlayer)
	Catapult_Mode_Total_score(CurrentPlayer) = 0
End Sub

Sub ModeTotalKnightChallenge_Timer()
	BypassVideo = False
	VideoIsPlayed = True
	ModeTotalKnightChallenge.Enabled = False
	HideOverlay = True
	VideoHideOverlay
	EndOfMode.Interval=4000
	EndOfMode.Enabled=True
	playmedia "TKC_ModeTotal.mp4","Catapult Multiball",pBackglass,"cineon",4000,"",1,20  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	If priority <= 20 Then
		pSplashCommentDisplayed "Multiball Total",3,255
		pSplashCommentDisplayed2 "Multiball Total",3,40
		pSplashAddScoreDisplayed "" & FormatNumber(AddScoreTripleKnightChallengeTotal(CurrentPlayer),0),3,255
		pSplashAddScoreDisplayed2 "" & FormatNumber(AddScoreTripleKnightChallengeTotal(CurrentPlayer),0),3,40
	End If
'	pupevent XXX
	AddScoreTripleKnightChallengeKnightHited = 1000000
	AddScoreTripleKnightChallengeGoldRoom = 1000000
	ScoreTripleKnightsMBChampion(CurrentPlayer) = ScoreTripleKnightsMBChampion(CurrentPlayer) + AddScoreTripleKnightChallengeTotal(CurrentPlayer)
	AddScoreTripleKnightChallengeTotal(CurrentPlayer) = 0
End Sub

Sub ModeTotalWarHurry_Timer()
	BypassVideo = False
	VideoIsPlayed = True
	ModeTotalWarHurry.Enabled = False
	HideOverlay = True
	VideoHideOverlay
	EndOfMode.Interval=7000
	EndOfMode.Enabled=True
	playmedia "SkeletonPopsBKCloseUp_BGLoop.mp4","Skeleton BG",pBackglass,"cineon",7000,"",1,20  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	If priority <= 20 Then
		pSplashCommentDisplayed "War Hurry Up Total",3,255
		pSplashCommentDisplayed2 "War Hurry Up Total",3,40
		pSplashAddScoreDisplayed "" & FormatNumber(AddScoreWarHurryUpTotal(CurrentPlayer),0),3,255
		pSplashAddScoreDisplayed2 "" & FormatNumber(AddScoreWarHurryUpTotal(CurrentPlayer),0),3,40
	End If
'	pupevent 572
	ScoreWarChampion(CurrentPlayer) = ScoreWarChampion(CurrentPlayer) + AddScoreWarHurryUpTotal(CurrentPlayer)
	AddScoreWarHurryUpTotal(CurrentPlayer) = 0
End Sub

Sub ModeTotalRetro_Timer()
	VideoIsPlayed = True
End Sub

Sub VideoCatapultLoopSetBackground_Timer()
	BypassVideo = False
	VideoCatapultLoopSetBackground.Enabled = False
	video_catapult_mode_loop
	If LastChanceFlag = True Then
		LastChanceCatapultMode = True
		DisableTable False
		bGameInPlay = True
	End If
End Sub

'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
' Why DOF Video_catapult mode loop is commented ??????
'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

Sub video_catapult_mode_loop
	BypassVideo = False
	CatapultManagement.interval = 500
	CatapultManagement.Enabled = True
	HideOverlay = False
	restoreOverlay.Interval=100
	restoreOverlay.Enabled=True
'			If vsvol = 100 Then 
'				DOF 522, DOFPulse
'				pupevent 522
'			ElseIf vsvol < 100 And vsvol > 89 Then pupevent 919
'			ElseIf vsvol < 90 And vsvol > 79  Then pupevent 918
'			ElseIf vsvol < 80 And vsvol > 69  Then pupevent 917
'			ElseIf vsvol < 70 And vsvol > 59  Then pupevent 916
'			ElseIf vsvol < 60 And vsvol > 49  Then pupevent 915
'			ElseIf vsvol < 50 And vsvol > 39  Then pupevent 914
'			ElseIf vsvol < 40 And vsvol > 29  Then pupevent 913
'			ElseIf vsvol < 30 And vsvol > 19  Then pupevent 912
'			Else pupevent 911
'			End If
    EnableBallSaver 40:bBallSaverActive = True
'	BackGroundFileIs = "BlackKnight_CatapultMultiball_LoopingMainplayBG.mp4"
'	BackGroundFolderIs = "Catapult Multiball"
'	BackGroundLenghtIs = 18000
'''	SetBackGroundGeneral

'	playmedia "BlackKnight_CatapultMultiball_LoopingMainplayBG.mp4","Catapult Multiball",pBackglass,"cineon",18000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	PuPlayer.setbackground pBackglass, 1  
'	EndLightsequenceAnimation.Enabled=True
	CatapultModeFlag = True
	BouleAfterHit.Enabled = True
	CheckLamp.Enabled = True
	Catapult_Super_Jackpot_Multi = 1
'	SetBackGroundImportant.Enabled = True
	VideoSetBackground
End Sub

Sub StopMainScreen
	PuPlayer.playstop pTopper
	PuPlayer.playstop pDMD
	PuPlayer.playstop pBackglass
	PuPlayer.playstop pPlayfield
	PuPlayer.playstop pMusic
	PuPlayer.playstop pAudio
	PuPlayer.playstop pCallouts
	PuPlayer.playstop pBackGlass2
	PuPlayer.playstop pPopSW71
	PuPlayer.playstop pPopSW72
	PuPlayer.playstop pPop
End Sub

Sub Video_lock1IsLit
	BypassVideo = False
	HideOverlay = True
	VideoHideOverlay
	EndOfVideo.Interval=4000
	EndOfVideo.Enabled=True
	playmedia "TKC_Ball1LockLIT.mp4","Knight challenge",pBackglass,"cineon",3000,"",1,17  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
priority = 17
'	pupevent 544
End Sub
Sub Video_lock2IsLit
	BypassVideo = False
	HideOverlay = True
	VideoHideOverlay
	EndOfVideo.Interval=3000
	EndOfVideo.Enabled=True
	playmedia "TKC_Ball2LockLIT.mp4","Knight challenge",pBackglass,"cineon",3000,"",1,17  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
priority = 17
'	pupevent 545
End Sub
Sub Video_lock3IsLit
	BypassVideo = False
	HideOverlay = True
	VideoHideOverlay
	EndOfVideo.Interval=3000
	EndOfVideo.Enabled=True
	playmedia "TKC_Ball3LockLIT.mp4","Knight challenge",pBackglass,"cineon",3000,"",1,17  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
priority = 17
'	pupevent 546
End Sub

Sub Video_knightball1lock
	DOF 601, DOFPulse
	BypassVideo = True
	HideOverlay = True
	VideoHideOverlay
	EndOfVideo.Interval=6000
	EndOfVideo.Enabled=True
	playmedia "TKC_Ball1Lock.mp4","Knight challenge",pBackglass,"cineon",6000,"",1,18  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
priority = 18
'	pupevent 547
End Sub
Sub Video_knightball2lock
	DOF 602, DOFPulse			  
	BypassVideo = True
	HideOverlay = True
	VideoHideOverlay
	EndOfVideo.Interval=6000
	EndOfVideo.Enabled=True
	playmedia "TKC_Ball2Lock.mp4","Knight challenge",pBackglass,"cineon",6000,"",1,18  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
priority = 18
'	pupevent 548
End Sub
Sub Video_knightball3lock
	DOF 603, DOFPulse
	DOF 604, DOFOn			   
	BypassVideo = True
	HideOverlay = True
	VideoHideOverlay
	EndOfVideo.Interval=6000
	EndOfVideo.Enabled=True
	'PlaySound "Sound-0x03A3"
	SpeakTime = 1780
	LightEyesBK
	playmedia "Sound-0x03A3.mp3","Audioknight",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	playmedia "TKC_Ball3Lock.mp4","Knight challenge",pBackglass,"cineon",6000,"",1,18  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
priority = 18
'	pupevent 549
End Sub

Sub video_knight_challenge_background(Knight)
	BypassVideo = False
	HideOverlay = False
	restoreOverlay.Interval=100
	restoreOverlay.Enabled=True
	If Knight_challenge_phase = 1 or Knight_challenge_phase = 2 Then
		If Knight = 0 Then
							  
			If vsvol = 100 Then 
				'DOF 559, DOFPulse
				pupevent 559
			ElseIf vsvol < 100 And vsvol > 89 Then pupevent 990
			ElseIf vsvol < 90 And vsvol > 79  Then pupevent 980
			ElseIf vsvol < 80 And vsvol > 69  Then pupevent 970
			ElseIf vsvol < 70 And vsvol > 59  Then pupevent 960
			ElseIf vsvol < 60 And vsvol > 49  Then pupevent 950
			ElseIf vsvol < 50 And vsvol > 39  Then pupevent 940
			ElseIf vsvol < 40 And vsvol > 29  Then pupevent 930
			ElseIf vsvol < 30 And vsvol > 19  Then pupevent 920
			Else pupevent 910
			End If
			'playmedia "TKC_jackpotREADYbkexposed.mp4","Knight challenge",pBackglass,"cineon",6000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			'pupevent 559
'			BackGroundFileIs = "TKC_jackpotREADYbkexposed.mp4"
'			BackGroundFolderIs = "Knight challenge"
'			BackGroundLenghtIs = 6000
'''			SetBackGroundGeneral
		End If
		If Knight = 1 Then
			'playmedia "TKC_1knightIdle.mp4","Knight challenge",pBackglass,"cineon",6000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
							 
			If vsvol = 100 Then 
'				DOF 550, DOFPulse
				pupevent 550
			ElseIf vsvol < 100 And vsvol > 89 Then pupevent 979
			ElseIf vsvol < 90 And vsvol > 79  Then pupevent 978
			ElseIf vsvol < 80 And vsvol > 69  Then pupevent 977
			ElseIf vsvol < 70 And vsvol > 59  Then pupevent 976
			ElseIf vsvol < 60 And vsvol > 49  Then pupevent 975
			ElseIf vsvol < 50 And vsvol > 39  Then pupevent 974
			ElseIf vsvol < 40 And vsvol > 29  Then pupevent 973
			ElseIf vsvol < 30 And vsvol > 19  Then pupevent 972
			Else pupevent 971
			End If
			'pupevent 550
'			BackGroundFileIs = "TKC_1knightIdle.mp4"
'			BackGroundFolderIs = "Knight challenge"
'			BackGroundLenghtIs = 6000
'''			SetBackGroundGeneral
		End If
		If Knight = 2 Then
			'playmedia "TKC_2knightIdle.mp4","Knight challenge",pBackglass,"cineon",6000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
							 
			If vsvol = 100 Then 
'				DOF 551, DOFPulse
				pupevent 551
			ElseIf vsvol < 100 And vsvol > 89 Then pupevent 989
			ElseIf vsvol < 90 And vsvol > 79  Then pupevent 988
			ElseIf vsvol < 80 And vsvol > 69  Then pupevent 987
			ElseIf vsvol < 70 And vsvol > 59  Then pupevent 986
			ElseIf vsvol < 60 And vsvol > 49  Then pupevent 985
			ElseIf vsvol < 50 And vsvol > 39  Then pupevent 984
			ElseIf vsvol < 40 And vsvol > 29  Then pupevent 983
			ElseIf vsvol < 30 And vsvol > 19  Then pupevent 982
			Else pupevent 981
			End If
			'pupevent 551
'			BackGroundFileIs = "TKC_2knightsIdle.mp4"
'			BackGroundFolderIs = "Knight challenge"
'			BackGroundLenghtIs = 6000
'''			SetBackGroundGeneral
		End If
		If Knight = 3 Then
			'playmedia "TKC_3knightIdle.mp4","Knight challenge",pBackglass,"cineon",6000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
							 
			If vsvol = 100 Then 
'				DOF 552, DOFPulse
				pupevent 552
			ElseIf vsvol < 100 And vsvol > 89 Then pupevent 999
			ElseIf vsvol < 90 And vsvol > 79  Then pupevent 998
			ElseIf vsvol < 80 And vsvol > 69  Then pupevent 997
			ElseIf vsvol < 70 And vsvol > 59  Then pupevent 996
			ElseIf vsvol < 60 And vsvol > 49  Then pupevent 995
			ElseIf vsvol < 50 And vsvol > 39  Then pupevent 994
			ElseIf vsvol < 40 And vsvol > 29  Then pupevent 993
			ElseIf vsvol < 30 And vsvol > 19  Then pupevent 992
			Else pupevent 991
			End If
			'pupevent 552
'			BackGroundFileIs = "TKC_3knightsIdle.mp4"
'			BackGroundFolderIs = "Knight challenge"
'			BackGroundLenghtIs = 6000
'''			SetBackGroundGeneral
		End If
	End If
	If Knight_challenge_phase = 3 Then
		'playmedia "TKC_GoldRoomIdle01.mp4","Knight challenge",pBackglass,"cineon",6000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'		BackGroundFileIs = "TKC_GoldRoomIdle01.mp4"
'		BackGroundFolderIs = "Knight challenge"
'		BackGroundLenghtIs = 6000
'''		SetBackGroundGeneral
							
			If vsvol = 100 Then 
'				DOF 557, DOFPulse
				pupevent 557
			ElseIf vsvol < 100 And vsvol > 89 Then pupevent 1009
			ElseIf vsvol < 90 And vsvol > 79  Then pupevent 1008
			ElseIf vsvol < 80 And vsvol > 69  Then pupevent 1007
			ElseIf vsvol < 70 And vsvol > 59  Then pupevent 1006
			ElseIf vsvol < 60 And vsvol > 49  Then pupevent 1005
			ElseIf vsvol < 50 And vsvol > 39  Then pupevent 1004
			ElseIf vsvol < 40 And vsvol > 29  Then pupevent 1003
			ElseIf vsvol < 30 And vsvol > 19  Then pupevent 1002
			Else pupevent 1001
			End If
		'pupevent 557


'		If x=1 Then
'			'playmedia "TKC_GoldRoomIdle02.mp4","Knight challenge",pBackglass,"cineon",6000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'			DOF 609, DOFPulse 'Strobe
'		End If
	End If
End Sub

Sub Video_knight_challenge_phase1_hited
	BypassVideo = False
	HideOverlay = True
	VideoHideOverlay
	'EndOfVideo.Interval=3000
	'EndOfVideo.Enabled=True
	If KnightRemaining => 1 Then
		playmedia "TKC_wrongKnight_skeleton.mp4","Knight challenge",pBackglass,"cineon",3000,"",1,10  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		priority = 10
'		pupevent 562
	Else
		playmedia "TKC_wrongKnight_BlackKnight.mp4","Knight challenge",pBackglass,"cineon",3000,"",1,11  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		priority = 11
'		pupevent 561
	End If
	'video_knight_challenge_background KnightRemaining
	KnightRemainingSetBackground.Interval=4000
	KnightRemainingSetBackground.Enabled = True
	pSplashAddScoreDisplayed "" & FormatNumber(AddScoreTripleKnightChallenge,0),3,255
	pSplashAddScoreDisplayed2 "" & FormatNumber(AddScoreTripleKnightChallenge,0),3,40
	AddScoreTripleKnightChallengeKnightHited = AddScoreTripleKnightChallengeKnightHited + 300000
End Sub

Sub KnightRemainingSetBackground_timer()
	KnightRemainingSetBackground.Enabled = False
	video_knight_challenge_background KnightRemaining
End Sub
			
sub video_knight_challenge_advance_to_gold_room
	BypassVideo = False
	HideOverlay = True
	VideoHideOverlay
	'EndOfVideo.Interval=4000
	'EndOfVideo.Enabled=True
	playmedia "TKC_AdvanceToGoldRoom.mp4","Knight challenge",pBackglass,"cineon",6000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	pupevent 553
	KnightRemainingSetBackground.Interval=6000
	KnightRemainingSetBackground.Enabled = True
	pSplashAddScoreDisplayed "" & FormatNumber(AddScoreTripleKnightChallenge,0),3,255
	pSplashAddScoreDisplayed2 "" & FormatNumber(AddScoreTripleKnightChallenge,0),3,40
	AddScoreTripleKnightChallengeKnightHited = AddScoreTripleKnightChallengeKnightHited + 300000
End Sub

sub video_knight_challenge_jackpot
	BypassVideo = False
	HideOverlay = True
	VideoHideOverlay
	EndOfVideo.Interval=4000
	EndOfVideo.Enabled=True
	If knight_challenge_jackpot = 1 Then
		playmedia "TKC_GoldRoomAward01.mp4","Knight challenge",pBackglass,"cineon",6000,"",1,10  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		priority = 10
'		pupevent 553
	End If
	If knight_challenge_jackpot = 2 Then
		playmedia "TKC_GoldRoomAward02.mp4","Knight challenge",pBackglass,"cineon",6000,"",1,10  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		priority = 10
'		pupevent 555
	End If
	pSplashAddScoreDisplayed "" & FormatNumber(AddScoreTripleKnightChallenge,0),3,255
	pSplashAddScoreDisplayed2 "" & FormatNumber(AddScoreTripleKnightChallenge,0),3,40
	AddScoreTripleKnightChallengeGoldRoom = AddScoreTripleKnightChallengeGoldRoom + 50000
End Sub

Sub Video_Mistery
	BypassVideo = False
	RandomMysterySelect1 = ""
	MysteryRandom
	RandomMysterySelect2 = ""
	MysteryRandom
	RandomMysterySelect3 = ""
	MysteryRandom
	RandomMysterySelect4 = ""
	MysteryRandom
	HideOverlay = True
	VideoHideOverlay
	EndOfVideo.Interval=5000
	EndOfVideo.Enabled=True
	'PlaySound "Sound-0x03A3"
	'playmedia "Sound-0x03A3.mp3","Audioknight",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	pSplashMystery "Mystery",1,30719
	playmedia "Mistery.mp4","Mistery",pBackglass,"cineon",4000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	pupevent 577
	RandomMystery1.Enabled = True
	RandomMystery2.Enabled = True
	RandomMystery3.Enabled = True
	RandomMystery4.Enabled = True
	MysteryFlag = False
	L96.State = 0
End Sub

Sub RandomMystery1_timer()
	RandomMystery1.Enabled = False
	pSplashMystery "", 1, 307717
	pSplashRandomMystery RandomMysterySelect1, 1,30719
End Sub

Sub RandomMystery2_timer()
	RandomMystery2.Enabled = False
	pSplashRandomMystery RandomMysterySelect2, 1,30719
End Sub
Sub RandomMystery3_timer()
	RandomMystery3.Enabled = False
	pSplashRandomMystery RandomMysterySelect3, 1,30719
End Sub
Sub RandomMystery4_timer()
	RandomMystery4.Enabled = False
	pSplashRandomMystery RandomMysterySelect4, 1,30719


	If RandomMysterySelect4 = "250,000" Then
		AddScore 250000
	ElseIf RandomMysterySelect4 = "750,000" Then
		AddScore 750000
	ElseIf RandomMysterySelect4 = "1,500,000" Then
		AddScore 1500000
	ElseIf RandomMysterySelect4 = "2,500,000" Then
		AddScore 2500000
	ElseIf RandomMysterySelect4 = "hold bonus" Then
'*		'''"hold bonus"	    												' I don't what is this????
'*
'*
'*
'*
'*
'*
'*
	ElseIf RandomMysterySelect4 = "light lock" Then
'*
'*
'*
'*
'*
'*
'*
	ElseIf RandomMysterySelect4 = "war hurry ups" Then
		WarHurryFlag = True
		WarHurryTimer.Enabled = True
		CheckLamp.Enabled = True
		WarHurryPhase = 1
		MusicCheck 66
		Video_WarHurryUp
	ElseIf RandomMysterySelect4 = "bonus X" Then
		WarCount = WarCount + 1
	ElseIf RandomMysterySelect4 = "knight letter" Then
		KnightLamp(CurrentPlayer) = KnightLamp(CurrentPlayer) + 2
	ElseIf RandomMysterySelect4 = "light super" Then
		SuperIsLit = True
	ElseIf RandomMysterySelect4 = "advance save" Then
		BallSaveAvailable(CurrentPlayer) = True
	ElseIf RandomMysterySelect4 = "light magna save" Then
		MagnaSaveFlag(CurrentPlayer) = 1
		DOF 201, DOFOn
	ElseIf RandomMysterySelect4 = "light extra ball" Then
		ExtraBallsIsLit = True
	End If
End Sub

Sub WarHurryTimer_Timer()
	L85State(CurrentPlayer) = 0
	WarHurryTimer.Enabled = False
	WarHurryPhase = 0
	WarHurryFlag = False
	ModeTotalWarHurry.Interval = 3500
	ModeTotalWarHurry.Enabled = True
	VideoSetBackground
	MusicCheck 22
End Sub

Sub MysteryRandom()
	Select Case Int(Rnd*13)+1
		Case 1 : RandomMysterySelect = "250,000"
		Case 2 : RandomMysterySelect = "750,000"
		Case 3 : RandomMysterySelect = "1,500,000"
		Case 4 : RandomMysterySelect = "2,500,000"
		Case 5 : RandomMysterySelect = "hold bonus"					' I don't what is this????
		Case 6 : RandomMysterySelect = "light lock"				' I don't what is this????
		Case 7 : RandomMysterySelect = "war hurry ups"
		Case 8 : RandomMysterySelect = "bonus X"
		Case 9 : RandomMysterySelect = "knight letter"
		Case 10 : RandomMysterySelect = "light super"
		Case 11 : RandomMysterySelect = "advance save"
		Case 12 : RandomMysterySelect = "light magna save"
		Case 13 : RandomMysterySelect = "light extra ball"		
	End Select
	CheckMysteryRandom
End Sub

Sub checkMysteryRandom
	If RandomMysterySelect1 = "" Then 
		RandomMysterySelect1 = RandomMysterySelect
	Elseif RandomMysterySelect1 = RandomMysterySelect Then
		MysteryRandom
	ElseIf RandomMysterySelect2 = "" Then 
		RandomMysterySelect2 = RandomMysterySelect
	Elseif RandomMysterySelect2 = RandomMysterySelect Then
		MysteryRandom
	ElseIf RandomMysterySelect3 = "" Then 
		RandomMysterySelect3 = RandomMysterySelect
	Elseif RandomMysterySelect3 = RandomMysterySelect Then
		MysteryRandom
	ElseIf RandomMysterySelect4 = "" Then 
		RandomMysterySelect4 = RandomMysterySelect
'	Elseif RandomMysterySelect1 = RandomMysterySelect Then
'		MysteryRandom
	End If
End Sub

Sub Video_Display_Match_Timer()
'	BypassVideo = True
	BypassVideo = False
	Video_Display_Match.Enabled=False
	playclear pMusic
	HideOverlay = False
'	VideoHideOverlay
	MusicCheck 0
	playmedia "BK_Match.mp4","Match",pBackglass,"cineon",6000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	pupevent 575
	ScorePercent(CurrentPlayer) = int(Score(CurrentPlayer) /100)
	ScoreCent(CurrentPlayer) = Score(CurrentPlayer) - (ScorePercent(CurrentPlayer) * 100)
	MatchRandom = RndNum(0,9) * 10
	DisplayMatchRandom.Interval=5200
	DisplayMatchRandom.Enabled = True
End Sub

Sub DisplayMatchRandom_timer()
	BypassVideo = False
'	BypassVideo = True
	EndOfVideoGameOver.Interval=4000
	EndOfVideoGameOver.Enabled=True
	DisplayMatchRandom.Enabled = False
	If PlayersPlayingGame = 1 Then
		ScorePercent(1) = int(Score(1) /100)
		ScoreCent(1) = (ScorePercent(1) * 100) + MatchRandom
		If Score(1) = ScoreCent(1) Then
			Credits=Credits + 1
			SaveCredits
'			Savehs
			Playsound "Sound-0x0006"
		End If
	End If
	If PlayersPlayingGame = 2 Then
		ScorePercent(1) = int(Score(1) /100)
		ScorePercent(2) = int(Score(2) /100)
		ScoreCent(1) = (ScorePercent(1) * 100) + MatchRandom
		ScoreCent(2) = (ScorePercent(2) * 100) + MatchRandom
		If Score(1) = ScoreCent(1) Or Score(2) = ScoreCent(2) Then
			Credits=Credits + 1
			SaveCredits
'			Savehs
			Playsound "Sound-0x0006"
		End If
	End If
	If PlayersPlayingGame = 3 Then
		ScorePercent(1) = int(Score(1) /100)
		ScorePercent(2) = int(Score(2) /100)
		ScorePercent(3) = int(Score(3) /100)
		ScoreCent(1) = (ScorePercent(1) * 100) + MatchRandom
		ScoreCent(2) = (ScorePercent(2) * 100) + MatchRandom
		ScoreCent(3) = (ScorePercent(3) * 100) + MatchRandom 
		If Score(1) = ScoreCent(1) Or Score(2) = ScoreCent(2) Or Score(3) = ScoreCent(3) Then
			Credits=Credits + 1
			SaveCredits
'			Savehs
			Playsound "Sound-0x0006"
		End If
	End If
	If PlayersPlayingGame = 4 Then
		ScorePercent(1) = int(Score(1) /100)
		ScorePercent(2) = int(Score(2) /100)
		ScorePercent(3) = int(Score(3) /100)
		ScorePercent(4) = int(Score(4) /100)
		ScoreCent(1) = (ScorePercent(1) * 100) + MatchRandom
		ScoreCent(2) = (ScorePercent(2) * 100) + MatchRandom
		ScoreCent(3) = (ScorePercent(3) * 100) + MatchRandom
		ScoreCent(4) = (ScorePercent(4) * 100) + MatchRandom
		If Score(1) = ScoreCent(1) Or Score(2) = ScoreCent(2) Or Score(3) = ScoreCent(3) Or Score(4) = ScoreCent(4) Then
			Credits=Credits + 1
			SaveCredits
'			Savehs
			Playsound "Sound-0x0006"
		End If
	End If
	MatchingScore
End Sub


'Sub Video_DisplayScore01	'Only Base Bonus
'	puPlayer.LabelSet pBackglass,"CommentDisplayed","",1,""
'	puPlayer.LabelSet pBackglass,"CommentDisplayed2","",1,""
'	puPlayer.LabelSet pBackglass,"AddScoreDisplayed","",1,""
'	puPlayer.LabelSet pBackglass,"AddScoreDisplayed2","",1,""
'
''	BypassVideo = True
'	HideOverlay = True
'	VideoHideOverlay
'	HideBonusScore.Interval=2000
'	HideBonusScore.Enabled=True
'	DisplayTotalScoreBonus.Interval=2500
'	DisplayTotalScoreBonus.Enabled=True
'	HideTotalBonusScore.Interval=4000
'	HideTotalBonusScore.Enabled=True
'	EndOfVideo.Interval=7000
'	EndOfVideo.Enabled=True
'	puPlayer.LabelSet pBackglass,"BaseBonus","" & "" & FormatNumber(BaseBonus,0),1,""
'	playmedia "score02.mp4","Score",pBackglass,"cineon",7000,"",1,100  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
''	pupevent 582
'End Sub

Sub Video_DisplayScore02	' Base Bonus, Loops, etc...
	puPlayer.LabelSet pBackglass,"CommentDisplayed","",1,""
	puPlayer.LabelSet pBackglass,"CommentDisplayed2","",1,""
	puPlayer.LabelSet pBackglass,"AddScoreDisplayed","",1,""
	puPlayer.LabelSet pBackglass,"AddScoreDisplayed2","",1,""
'	BypassVideo = True
	BypassVideo = False
	HideOverlay = True
	VideoHideOverlay
	EndOfVideo.Interval=7500
	EndOfVideo.Enabled=True
	displaySuperFeatures.interval=1200
	displaySuperFeatures.Enabled=True
	displayCombos.interval=1800
	displayCombos.Enabled=True
	displayLoops.interval=2400
	displayLoops.Enabled=True
	displayBonusMultiplier.interval=3400
	displayBonusMultiplier.Enabled=True
	HideBonusScore.Interval=4500
	HideBonusScore.Enabled=True
	DisplayTotalScoreBonus.Interval=4700
	DisplayTotalScoreBonus.Enabled=True
	HideTotalBonusScore.Interval=5000
	HideTotalBonusScore.Enabled=True
	PlaySound "Sound-0x0008", 0, SongVolume
	'playmedia "Sound-0x00BF.mp3","Audionoise",pAudio,"",0,"",1,1  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	playmedia "score02.mp4","Score",pBackglass,"cineon",7000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	pupevent 582
	puPlayer.LabelSet pBackglass,"TitleBonus","bonus",1,""
	puPlayer.LabelSet pBackglass,"TextBaseBonus","base bonus",1,""
	puPlayer.LabelSet pBackglass,"basebonus","" & "" & FormatNumber(BaseBonus,0),1,""
End Sub

Sub displaySuperFeatures_timer()
'	BypassVideo = True
	BypassVideo = False
	displaySuperFeatures.Enabled=False
	puPlayer.LabelSet pBackglass,"TextSuperFeatures","super features",1,""
	puPlayer.LabelSet pBackglass,"SuperFeatures","" & FormatNumber(SuperFeatures,0),1,""
'	PlaySound "Sound-0x0008"
	PlaySound "Sound-0x0008", 0, SongVolume
'	playmedia "Sound-0x00BF.mp3","Audionoise",pAudio,"",0,"",1,1  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
End Sub

Sub displayCombos_timer()
	Combos = 25000 * Uturn
	ScoreComboChampion(CurrentPlayer) = ScoreComboChampion(CurrentPlayer) + Uturn
'	Combos = Uturn(CurrentPlayer)

'	BypassVideo = True
	BypassVideo = False
	displayCombos.Enabled=False
	puPlayer.LabelSet pBackglass,"TextCombos","combos",1,""
	puPlayer.LabelSet pBackglass,"Combos","" & FormatNumber(Combos,0),1,""
'	PlaySound "Sound-0x0008"
	PlaySound "Sound-0x0008", 0, SongVolume
	'playmedia "Sound-0x00BF.mp3","Audionoise",pAudio,"",0,"",1,1  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
End Sub

Sub displayLoops_timer()
'	BypassVideo = True
	BypassVideo = False
	displayLoops.Enabled=False
	puPlayer.LabelSet pBackglass,"TextLoops","loops",1,""
	puPlayer.LabelSet pBackglass,"Loops","" & FormatNumber(Loops,0),1,""
'	PlaySound "Sound-0x0008"
	PlaySound "Sound-0x0008", 0, SongVolume
'	playmedia "Sound-0x00BF.mp3","Audionoise",pAudio,"",0,"",1,1  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
End Sub

Sub displayBonusMultiplier_timer()
'	BypassVideo = True
	BypassVideo = False
	displayBonusMultiplier.Enabled=False
	puPlayer.LabelSet pBackglass,"TitleBonus","",1,""
	puPlayer.LabelSet pBackglass,"TextBaseBonus","",1,""
	puPlayer.LabelSet pBackglass,"TextSuperFeatures","",1,""
	puPlayer.LabelSet pBackglass,"TextCombos","",1,""
	puPlayer.LabelSet pBackglass,"TextLoops","",1,""
	puPlayer.LabelSet pBackglass,"BaseBonus","",1,""
	puPlayer.LabelSet pBackglass,"SuperFeatures","",1,""
	puPlayer.LabelSet pBackglass,"Combos","",1,""
	puPlayer.LabelSet pBackglass,"Loops","",1,""
	puPlayer.LabelSet pBackglass,"BonusMultiplier",FormatNumber(BonusMultiplier(CurrentPlayer),0)&"X",1,""
'	PlaySound "Sound-0x0008"
	PlaySound "Sound-0x0008", 0, SongVolume
'	playmedia "Sound-0x00BF.mp3","Audionoise",pAudio,"",0,"",1,1  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
End Sub

Sub HideBonusScore_timer()
'	BypassVideo = True
	BypassVideo = False
	HideBonusScore.Enabled=False
	TotalBonus = (BaseBonus + SuperFeatures + Combos + Loops) * BonusMultiplier(CurrentPlayer)
	ScoreBonusChampion(CurrentPlayer) = ScoreBonusChampion(CurrentPlayer) + TotalBonus
	puPlayer.LabelSet pBackglass,"TitleBonus","",1,""
	puPlayer.LabelSet pBackglass,"TextBaseBonus","",1,""
	puPlayer.LabelSet pBackglass,"TextSuperFeatures","",1,""
	puPlayer.LabelSet pBackglass,"TextCombos","",1,""
	puPlayer.LabelSet pBackglass,"TextLoops","",1,""
	puPlayer.LabelSet pBackglass,"BaseBonus","",1,""
	puPlayer.LabelSet pBackglass,"SuperFeatures","",1,""
	puPlayer.LabelSet pBackglass,"Combos","",1,""
	puPlayer.LabelSet pBackglass,"Loops","",1,""
	puPlayer.LabelSet pBackglass,"BonusMultiplier","",1,""
	puPlayer.LabelSet pBackglass,"TextTotalBonus","total",1,""
	puPlayer.LabelSet pBackglass,"TotalBonus","" & FormatNumber(TotalBonus,0),1,""
'	PlaySound "Sound-0x0008"
	PlaySound "Sound-0x0008", 0, SongVolume
	'playmedia "Sound-0x00BF.mp3","Audionoise",pAudio,"",0,"",1,1  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	BaseBonus = 0
	SuperFeatures = 0
	Combos = 0
	Uturn = 0
	Loops = 0
	RageCount = 0
	WarCount = 1
End Sub
Sub DisplayTotalScoreBonus_timer()
'	BypassVideo = True
	BypassVideo = False
	DisplayTotalScoreBonus.Enabled=False
	Score(CurrentPlayer) = Score(CurrentPlayer) + TotalBonus
	puPlayer.LabelSet pBackglass,"TotalScoreBonus","" & FormatNumber(Score(CurrentPlayer),0),1,""
	playmedia "Sound-0x013D.mp3","Audionoise",pAudio,"",0,"",1,1  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
End Sub
Sub HideTotalBonusScore_timer()
	BypassVideo = False
	HideTotalBonusScore.Enabled=False
	'Score(CurrentPlayer) = Score(CurrentPlayer) + BaseBonus
	puPlayer.LabelSet pBackglass,"BonusMultiplier","",1,""
	puPlayer.LabelSet pBackglass,"TextTotalBonus","",1,""
	puPlayer.LabelSet pBackglass,"TotalBonus","",1,""
	puPlayer.LabelSet pBackglass,"TotalScoreBonus","",1,""
'	PlaySound "knocker"
End Sub

Sub VideoSetBackground
	BypassVideo = False
	HideOverlay = False
	playclear pBackglass
	EndOfVideo.Interval=100
	EndOfVideo.Enabled=True
'	Fireanimation.Enabled = True
'	pupevent 505
'	pupevent 505
	If CatapultModeFlag = True Then
		If vsvol = 100 Then 
'			DOF 522, DOFPulse
			pupevent 522
		ElseIf vsvol < 100 And vsvol > 89 Then pupevent 919
		ElseIf vsvol < 90 And vsvol > 79  Then pupevent 918
		ElseIf vsvol < 80 And vsvol > 69  Then pupevent 917
		ElseIf vsvol < 70 And vsvol > 59  Then pupevent 916
		ElseIf vsvol < 60 And vsvol > 49  Then pupevent 915
		ElseIf vsvol < 50 And vsvol > 39  Then pupevent 914
		ElseIf vsvol < 40 And vsvol > 29  Then pupevent 913
		ElseIf vsvol < 30 And vsvol > 19  Then pupevent 912
		Else pupevent 911
		End If
	Elseif Knight_challenge_flag = True Then
		video_knight_challenge_background KnightRemaining
	Elseif CurrentMissionFlag(CurrentPlayer) = 1 Then
		If MudBog(CurrentPlayer) = 2 Then
'			BackGroundFileIs = "BlackKnight_HydraBattle_BGIdle.mp4"
'			BackGroundFolderIs = "Hydra Mode"
'			BackGroundLenghtIs = 16000
					 
			If vsvol = 100 Then 
'				DOF 541, DOFPulse
				pupevent 541
			ElseIf vsvol < 100 And vsvol > 89 Then pupevent 949
			ElseIf vsvol < 90 And vsvol > 79  Then pupevent 948
			ElseIf vsvol < 80 And vsvol > 69  Then pupevent 947
			ElseIf vsvol < 70 And vsvol > 59  Then pupevent 946
			ElseIf vsvol < 60 And vsvol > 49  Then pupevent 945
			ElseIf vsvol < 50 And vsvol > 39  Then pupevent 944
			ElseIf vsvol < 40 And vsvol > 29  Then pupevent 943
			ElseIf vsvol < 30 And vsvol > 19  Then pupevent 942
			Else pupevent 941
			End If
		End If
		If MoltenFire(CurrentPlayer) = 2 Then
'			BackGroundFileIs = "BlackKnight_FireElementalBattle_LoopingMainplayBackground.mp4"
'			BackGroundFolderIs = "Fire Elemental Mode"
'			BackGroundLenghtIs = 15000
					 
			If vsvol = 100 Then 
'				DOF 533, DOFPulse
				pupevent 533
			ElseIf vsvol < 100 And vsvol > 89 Then pupevent 929
			ElseIf vsvol < 90 And vsvol > 79  Then pupevent 928
			ElseIf vsvol < 80 And vsvol > 69  Then pupevent 927
			ElseIf vsvol < 70 And vsvol > 59  Then pupevent 926
			ElseIf vsvol < 60 And vsvol > 49  Then pupevent 925
			ElseIf vsvol < 50 And vsvol > 39  Then pupevent 924
			ElseIf vsvol < 40 And vsvol > 29  Then pupevent 923
			ElseIf vsvol < 30 And vsvol > 19  Then pupevent 922
			Else pupevent 921
			End If
		End If
		If BurningSands(CurrentPlayer) = 2 Then
'			BackGroundFileIs = "BK_WheelMode_SandWorm_BGIdle.mp4"
'			BackGroundFolderIs = "Sandworm Mode"
'			BackGroundLenghtIs = 18000
					 
			If vsvol = 100 Then 
'				DOF 580, DOFPulse
				pupevent 580
			ElseIf vsvol < 100 And vsvol > 89 Then pupevent 969
			ElseIf vsvol < 90 And vsvol > 79  Then pupevent 968
			ElseIf vsvol < 80 And vsvol > 69  Then pupevent 967
			ElseIf vsvol < 70 And vsvol > 59  Then pupevent 966
			ElseIf vsvol < 60 And vsvol > 49  Then pupevent 965
			ElseIf vsvol < 50 And vsvol > 39  Then pupevent 964
			ElseIf vsvol < 40 And vsvol > 29  Then pupevent 963
			ElseIf vsvol < 30 And vsvol > 19  Then pupevent 962
			Else pupevent 961
			End If
		End If
		If WickedCavern(CurrentPlayer) = 2 Then
'			BackGroundFileIs = "BK_WheelMode_Handholder_BGIdle.mp4"
'			BackGroundFolderIs = "Hand holder Mode"
'			BackGroundLenghtIs = 20000
					 
			If vsvol = 100 Then 
'				DOF 537, DOFPulse
				pupevent 537
			ElseIf vsvol < 100 And vsvol > 89 Then pupevent 939
			ElseIf vsvol < 90 And vsvol > 79  Then pupevent 938
			ElseIf vsvol < 80 And vsvol > 69  Then pupevent 937
			ElseIf vsvol < 70 And vsvol > 59  Then pupevent 936
			ElseIf vsvol < 60 And vsvol > 49  Then pupevent 935
			ElseIf vsvol < 50 And vsvol > 39  Then pupevent 934
			ElseIf vsvol < 40 And vsvol > 29  Then pupevent 933
			ElseIf vsvol < 30 And vsvol > 19  Then pupevent 932
			Else pupevent 931
			End If
		End If
		If DeepFreeze(CurrentPlayer) = 2 Then
'			BackGroundFileIs = "BK_WheelMode_Lich_BGIdle.mp4"
'			BackGroundFolderIs = "Lich Mode"
'			BackGroundLenghtIs = 20000
					 
			If vsvol = 100 Then 
'				DOF 571, DOFPulse
				pupevent 571
			ElseIf vsvol < 100 And vsvol > 89 Then pupevent 959
			ElseIf vsvol < 90 And vsvol > 79  Then pupevent 958
			ElseIf vsvol < 80 And vsvol > 69  Then pupevent 957
			ElseIf vsvol < 70 And vsvol > 59  Then pupevent 956
			ElseIf vsvol < 60 And vsvol > 49  Then pupevent 955
			ElseIf vsvol < 50 And vsvol > 39  Then pupevent 954
			ElseIf vsvol < 40 And vsvol > 29  Then pupevent 953
			ElseIf vsvol < 30 And vsvol > 19  Then pupevent 952
			Else pupevent 951
			End If		
		End If
		If BlackCastle(CurrentPlayer) = 2 Then
'			BackGroundFileIs = "BlackKnight_BKBattle_LoopingMainplayBackground.mp4"
'			BackGroundFolderIs = "Black Castle Mode"
'			BackGroundLenghtIs = 20000
			If vsvol = 100 Then 
'				DOF 513, DOFPulse
				pupevent 513
			ElseIf vsvol < 100 And vsvol > 89 Then pupevent 909
			ElseIf vsvol < 90 And vsvol > 79  Then pupevent 908
			ElseIf vsvol < 80 And vsvol > 69  Then pupevent 907
			ElseIf vsvol < 70 And vsvol > 59  Then pupevent 906
			ElseIf vsvol < 60 And vsvol > 49  Then pupevent 905
			ElseIf vsvol < 50 And vsvol > 39  Then pupevent 904
			ElseIf vsvol < 40 And vsvol > 29  Then pupevent 903
			ElseIf vsvol < 30 And vsvol > 19  Then pupevent 902
			Else pupevent 901
			End If
		End If
	Else
		Select Case Int(Rnd*3)+1
			Case 1 : VideoSelected = "MainplayV1.mp4"
			Case 2 : VideoSelected = "MainplayV2.mp4"
			Case 3 : VideoSelected = "MainplayV3.mp4"
		End Select
'		BackGroundFileIs = VideoSelected
'		BackGroundFolderIs = "Background main play"
'		BackGroundLenghtIs = 4000
'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
'		DOF 505, DOFPulse
		pupevent 505
'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	End If
'	SetBackGroundImportant.Enabled = True
	
'''	SetBackGroundGeneral
'	playmedia VideoSelected,"Background main play",pBackglass,"cineon",4000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	PuPlayer.setbackground pBackglass, 1  
End Sub

Sub VideoBlackCastleIntro
	BypassVideo = False
	HideOverlay = True
	EndOfVideo.Interval=15000
	EndOfVideo.Enabled=True	
	VideoHideOverlay
	playmedia "BlackKnight_BKBattle_ModeIntro.mp4","Black Castle Mode",pBackglass,"cineon",15000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	pupevent 512
End Sub

Sub VideoBlackCastleLoop
	BypassVideo = False
	HideOverlay = False
	restoreOverlay.Interval=100
	restoreOverlay.Enabled=True
				   
	If vsvol = 100 Then 
'		DOF 513, DOFPulse
		pupevent 513
	ElseIf vsvol < 100 And vsvol > 89 Then pupevent 909
	ElseIf vsvol < 90 And vsvol > 79  Then pupevent 908
	ElseIf vsvol < 80 And vsvol > 69  Then pupevent 907
	ElseIf vsvol < 70 And vsvol > 59  Then pupevent 906
	ElseIf vsvol < 60 And vsvol > 49  Then pupevent 905
	ElseIf vsvol < 50 And vsvol > 39  Then pupevent 904
	ElseIf vsvol < 40 And vsvol > 29  Then pupevent 903
	ElseIf vsvol < 30 And vsvol > 19  Then pupevent 902
	Else pupevent 901
	End If
'	SetBackGroundImportant.Enabled = True
'	pupevent 513

'	BackGroundFileIs = "BlackKnight_BKBattle_LoopingMainplayBackground.mp4"
'	BackGroundFolderIs = "Black Castle Mode"
'	BackGroundLenghtIs = 20000
'''	SetBackGroundGeneral

'	playmedia "BlackKnight_BKBattle_LoopingMainplayBackground.mp4","Black Castle Mode",pBackglass,"cineon",20000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	PuPlayer.setbackground pBackglass, 1  
End Sub

Sub VideoHideOverlay
	playmedia "overlay_blank.png","PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	PuPlayer.LabelSet pBackglass,"AddScoreChest","",1,"{'mt':2,'color':28671, 'size': 5, 'xpos': 14, 'xalign': 1, 'ypos': 69, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"TimerSuperMode","",1,"{'mt':2,'color':16777215, 'size': 6, 'xpos': 5, 'xalign': 1, 'ypos': 9, 'yalign': 1}"
	puPlayer.LabelSet pBackglass,"LastChanceBallToLock","",1,""
	puPlayer.LabelSet pBackglass,"LastChanceTime","",1,""
	puPlayer.LabelSet pBackglass,"LastChanceJackpot","",1,""
	pSplashCommentDisplayed "",3,255
	pSplashCommentDisplayed2 "",3,40
	pSplashAddScoreDisplayed "",3,255
	pSplashAddScoreDisplayed2 "",3,40
End Sub

Sub VideoFireElementattack_timer()
	VideoFireElementattack.Enabled = False
'	playclear pBackglass
	BypassVideo = False
	HideOverlay = True
	VideoHideOverlay
	If ExtraBallAfterMonsterDefeated(CurrentPlayer) > 0 And  MoltenFireDefeated(CurrentPlayer) => 7 Then
		VideoExtraBallIsLit
	Else
		If CatapultModeFlag = False And Knight_challenge_flag = False Then
			EndOfVideo.Interval=3000
			EndOfVideo.Enabled=True
			Select Case Int(Rnd*4)+1
				Case 1 : VideoSelected = "BlackKnight_FireElementalBattle_AttackVariations_1.mp4"
				Case 2 : VideoSelected = "BlackKnight_FireElementalBattle_AttackVariations_2.mp4"
				Case 3 : VideoSelected = "BlackKnight_FireElementalBattle_AttackVariations_3.mp4"
				Case 4 : VideoSelected = "BlackKnight_FireElementalBattle_AttackVariations_4.mp4"
			End Select
			playmedia VideoSelected,"Fire elemental attack",pBackglass,"cineon",3000,"",1,10  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			priority = 10
	'		pupevent 531
			If priority <= 10 Then
				pSplashCommentDisplayed ""& CommentDisplayed,3,255
				pSplashCommentDisplayed2 ""& CommentDisplayed,3,40
				pSplashAddScoreDisplayed "" & FormatNumber(AddScoreMoltenFire,0),3,255
				pSplashAddScoreDisplayed2 "" & FormatNumber(AddScoreMoltenFire,0),3,40
			End If
		End If
	End If
	MoltenFireDefeated(CurrentPlayer) = MoltenFireDefeated(CurrentPlayer) + 1
	AddTimeForMission(CurrentPlayer) = AddTimeForMission(CurrentPlayer) + 15
	AddScoreMoltenFire = AddScoreMoltenFire + 274000
End Sub
Sub VideoHandHolderattack_timer()
	VideoHandHolderattack.Enabled = False
'	playclear pBackglass
	BypassVideo = False
	HideOverlay = True
	VideoHideOverlay

	If ExtraBallAfterMonsterDefeated(CurrentPlayer) > 0  And WickedCavernDefeated(CurrentPlayer) => 7 Then
		VideoExtraBallIsLit
	Else
		If CatapultModeFlag = False And Knight_challenge_flag = False Then
			EndOfVideo.Interval=3000
			EndOfVideo.Enabled=True
			Select Case Int(Rnd*3)+1
				Case 1 : VideoSelected = "BlackKnight_HandHolderBattle_AttackVariations_1.mp4"
				Case 2 : VideoSelected = "BlackKnight_HandHolderBattle_AttackVariations_2.mp4"
				Case 3 : VideoSelected = "BlackKnight_HandHolderBattle_AttackVariations_3.mp4"
				Case 4 : VideoSelected = "BlackKnight_HandHolderBattle_AttackVariations_4.mp4"
			End Select
			playmedia VideoSelected,"Hand holder attack",pBackglass,"cineon",3000,"",1,10  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			If priority <= 10 Then
				pSplashCommentDisplayed ""& CommentDisplayed,3,255
				pSplashCommentDisplayed2 ""& CommentDisplayed,3,40
				pSplashAddScoreDisplayed "" & FormatNumber(AddScoreWickedCavern,0),3,255
				pSplashAddScoreDisplayed2 "" & FormatNumber(AddScoreWickedCavern,0),3,40
			End If
		End If
	End If
'	pupevent 535
	WickedCavernDefeated(CurrentPlayer) = WickedCavernDefeated(CurrentPlayer) +1
	AddScoreWickedCavern = AddScoreWickedCavern + 250000
	AddTimeForMission(CurrentPlayer) = AddTimeForMission(CurrentPlayer) + 15
End Sub

Sub VideoExtraBallIsLit
	BypassVideo = False
	ExtraBallAfterMonsterDefeated(CurrentPlayer) = ExtraBallAfterMonsterDefeated(CurrentPlayer) - 1
'	HideOverlay = True
'	VideoHideOverlay
	EndOfVideo.Interval=6000
	EndOfVideo.Enabled=True
	playmedia "ExtraballIsLit.mp4","Extra ball",pBackglass,"cineon",6000,"",1,30  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	playmedia "Sound-0x08CD.mp3","Audiocomment",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	ExtraBallsIsLit = True
End Sub


Sub VideoExtraBall
	BypassVideo = False
	HideOverlay = True
	VideoHideOverlay
	EndOfVideo.Interval=8500
	EndOfVideo.Enabled=True
	playmedia "ExtraBall.mp4","Extra ball",pBackglass,"cineon",8500,"",1,30  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	playmedia "Sound-0x08AB.mp3","Audiocomment",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
End Sub

Sub VideoExtraBallShootAgain
	BypassVideo = False
	HideOverlay = True
	VideoHideOverlay
	EndOfVideo.Interval=5000
	EndOfVideo.Enabled=True
	playmedia "ShootAgain.mp4","Extra ball",pBackglass,"cineon",5000,"",1,18  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	playmedia "shoot again Sound-0x091A.mp3","Audiocomment",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
End Sub

Sub VideoHydraattack_timer()
	VideoHydraattack.Enabled = False
'	playclear pBackglass
	BypassVideo = False
	HideOverlay = True
	VideoHideOverlay
	If ExtraBallAfterMonsterDefeated(CurrentPlayer) > 0 And MudBogDefeated(CurrentPlayer) => 4 And CommentDisplayed = "Hydra Sealed!" Then
		VideoExtraBallIsLit
	Else
		If CatapultModeFlag = False And Knight_challenge_flag = False Then
	'		EndOfVideo.Interval=3000
			Select Case Int(Rnd*3)+1
				Case 1 : VideoSelected = "BlackKnight_HydraBattle_AttackVariations_1.mp4" : EndOfVideo.Interval=3000
				Case 2 : VideoSelected = "BlackKnight_HydraBattle_AttackVariations_2.mp4" : EndOfVideo.Interval=3000
				Case 3 : VideoSelected = "BlackKnight_HydraBattle_AttackVariations_3.mp4" : EndOfVideo.Interval=3000
				Case 4 : VideoSelected = "BlackKnight_HydraBattle_AttackVariations_4.mp4" : EndOfVideo.Interval=4000
			End Select
			EndOfVideo.Enabled=True
			playmedia VideoSelected,"Hydra attack",pBackglass,"cineon",4000,"",1,10  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	'		priority = 10
	'		puPlayer.LabelSet pBackglass,"CommentDisplayed","" & CommentDisplayed,1,""
	'		puPlayer.LabelSet pBackglass,"CommentDisplayed2","" & CommentDisplayed,1,""
	'		puPlayer.LabelSet pBackglass,"AddScoreDisplayed","" & FormatNumber(AddScoreMudBog,0),1,""
	'		puPlayer.LabelSet pBackglass,"AddScoreDisplayed2","" & FormatNumber(AddScoreMudBog,0),1,""
			If priority <= 10 Then
				pSplashCommentDisplayed ""& CommentDisplayed,3,255
				pSplashCommentDisplayed2 ""& CommentDisplayed,3,40
				pSplashAddScoreDisplayed "" & FormatNumber(AddScoreMudBog,0),3,255
				pSplashAddScoreDisplayed2 "" & FormatNumber(AddScoreMudBog,0),3,40
			End If
		End If
	End If

'	pupevent 539
	If CommentDisplayed = "Hydra Sealed!" Then
		MudBogDefeated(CurrentPlayer) = MudBogDefeated(CurrentPlayer) + 1
	End If
	AddTimeForMission(CurrentPlayer) = AddTimeForMission(CurrentPlayer) + 15
End Sub

Sub VideoLichattack_timer()
	VideoLichattack.Enabled = False
'	playclear pBackglass
	BypassVideo = False
	HideOverlay = True
	VideoHideOverlay

	If ExtraBallAfterMonsterDefeated(CurrentPlayer) > 0 And DeepFreezeDefeated(CurrentPlayer) => 4 Then
		VideoExtraBallIsLit
	Else
		If CatapultModeFlag = False And Knight_challenge_flag = False Then
			EndOfVideo.Interval=4000
			EndOfVideo.Enabled=True
			Select Case Int(Rnd*4)+1
				Case 1 : VideoSelected = "BlackKnight_LichBattle_AttackVariations_1.mp4"
				Case 2 : VideoSelected = "BlackKnight_LichBattle_AttackVariations_2.mp4"
				Case 3 : VideoSelected = "BlackKnight_LichBattle_AttackVariations_3.mp4"
				Case 4 : VideoSelected = "BlackKnight_LichBattle_AttackVariations_4.mp4"
			End Select
			playmedia VideoSelected,"Licht battle attack",pBackglass,"cineon",3000,"",1,10  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		'	priority = 10
		'	pupevent 573
			'DeepFreezeDefeated(CurrentPlayer) = DeepFreezeDefeated(CurrentPlayer) + 1
			If priority <= 10 Then
				pSplashCommentDisplayed ""& CommentDisplayed,3,255
				pSplashCommentDisplayed2 ""& CommentDisplayed,3,40
				pSplashAddScoreDisplayed "" & FormatNumber(AddScoreDeepFreeze,0),3,255
				pSplashAddScoreDisplayed2 "" & FormatNumber(AddScoreDeepFreeze,0),3,40
			End If
		End If
	End If
	AddTimeForMission(CurrentPlayer) = AddTimeForMission(CurrentPlayer) + 15
End Sub
Sub VideoSandwormattack_timer()
	VideoSandwormattack.Enabled = False
'	playclear pBackglass
	BypassVideo = False
	HideOverlay = True
	VideoHideOverlay
	If ExtraBallAfterMonsterDefeated(CurrentPlayer) > 0  And BurningSandsDefeated(CurrentPlayer) => 5 Then
		VideoExtraBallIsLit
	Else
		If CatapultModeFlag = False And Knight_challenge_flag = False Then
			EndOfVideo.Interval=3000
			EndOfVideo.Enabled=True
			Select Case Int(Rnd*4)+1
				Case 1 : VideoSelected = "BlackKnight_SandWormBattle_AttackVariations_1.mp4"
				Case 2 : VideoSelected = "BlackKnight_SandWormBattle_AttackVariations_2.mp4"
				Case 3 : VideoSelected = "BlackKnight_SandWormBattle_AttackVariations_3.mp4"
				Case 4 : VideoSelected = "BlackKnight_SandWormBattle_AttackVariations_4.mp4"
			End Select
			playmedia VideoSelected,"sandworm attack",pBackglass,"cineon",3000,"",1,10  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			'priority = 10
			If priority <= 10 Then
				pSplashCommentDisplayed ""& CommentDisplayed,3,255
				pSplashCommentDisplayed2 ""& CommentDisplayed,3,40
				pSplashAddScoreDisplayed "" & FormatNumber(AddScoreSandWorm,0),3,255
				pSplashAddScoreDisplayed2 "" & FormatNumber(AddScoreSandWorm,0),3,40
			End If
		End If
	End If
'	pupevent 578
	AddScoreSandWorm = AddScoreSandWorm + 250000
	BurningSandsDefeated(CurrentPlayer) = BurningSandsDefeated(CurrentPlayer) +1
	AddTimeForMission(CurrentPlayer) = AddTimeForMission(CurrentPlayer) + 15
End Sub

Sub VideoBlackCastleattack_timer()
	VideoBlackCastleattack.Enabled = False
	BypassVideo = False
	HideOverlay = True
	VideoHideOverlay
	EndOfVideo.Interval=3000
	EndOfVideo.Enabled=True
	Select Case Int(Rnd*2)+1
		Case 1 : VideoSelected = "BlackKnight_BKBattle_Attack_1.mp4"
		Case 2 : VideoSelected = "BlackKnight_BKBattle_Attack_2.mp4"
	End Select
	playmedia VideoSelected,"Black Castle Attack",pBackglass,"cineon",3000,"",1,10  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	priority = 10
'	pupevent 511
	'BlackCastleDefeated(CurrentPlayer) = BlackCastleDefeated(CurrentPlayer) +1
	AddTimeForMission(CurrentPlayer) = AddTimeForMission(CurrentPlayer) + 15
End Sub

Sub video_SavedBall
	BypassVideo = False
	HideOverlay = True
	VideoHideOverlay
	EndOfVideo.Interval=4000
	EndOfVideo.Enabled=True
	playmedia "BallSave.mp4","Ball Save",pBackglass,"cineon",4000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	pupevent 506
End Sub

Sub Video_Knight
	BypassVideo = False
	'KNIGHTVideo = 
	HideOverlay = True
	VideoHideOverlay
	If KnightLamp(CurrentPlayer) = 11 Then 		'all video are not the same duration - If it's the laste video the delay is 3 sec.
		DelayknightLamp = 4000
	Else
		DelayknightLamp = 2000
	End If
	EndOfVideo.Interval=DelayknightLamp
	EndOfVideo.Enabled=True
	If KnightLamp(CurrentPlayer) = 1 Then playmedia "BK_Spellout_K.mp4","Knight letter",pBackglass,"cineon",1000,"",1,15 : ScoreKnightChampion(CurrentPlayer) = ScoreKnightChampion(CurrentPlayer) + 1 : priority = 15: KnightLamp(CurrentPlayer) = KnightLamp(CurrentPlayer) + 1 End If'(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	If KnightLamp(CurrentPlayer) = 3 Then playmedia "BK_Spellout_N.mp4","Knight letter",pBackglass,"cineon",1000,"",1,15 : ScoreKnightChampion(CurrentPlayer) = ScoreKnightChampion(CurrentPlayer) + 1 : priority = 15: KnightLamp(CurrentPlayer) = KnightLamp(CurrentPlayer) + 1 End If'(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	If KnightLamp(CurrentPlayer) = 5 Then playmedia "BK_Spellout_I.mp4","Knight letter",pBackglass,"cineon",1000,"",1,15 : ScoreKnightChampion(CurrentPlayer) = ScoreKnightChampion(CurrentPlayer) + 1 : priority = 15: KnightLamp(CurrentPlayer) = KnightLamp(CurrentPlayer) + 1 End If'(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	If KnightLamp(CurrentPlayer) = 7 Then playmedia "BK_Spellout_G.mp4","Knight letter",pBackglass,"cineon",1000,"",1,15 : ScoreKnightChampion(CurrentPlayer) = ScoreKnightChampion(CurrentPlayer) + 1 : priority = 15: KnightLamp(CurrentPlayer) = KnightLamp(CurrentPlayer) + 1 End If'(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	If KnightLamp(CurrentPlayer) = 9 Then playmedia "BK_Spellout_H.mp4","Knight letter",pBackglass,"cineon",1000,"",1,15 : ScoreKnightChampion(CurrentPlayer) = ScoreKnightChampion(CurrentPlayer) + 1 : priority = 15: KnightLamp(CurrentPlayer) = KnightLamp(CurrentPlayer) + 1 End If'(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	If KnightLamp(CurrentPlayer) = 11 Then playmedia "BK_Spellout_T.mp4","Knight letter",pBackglass,"cineon",3000,"",1,15 : ScoreKnightChampion(CurrentPlayer) = ScoreKnightChampion(CurrentPlayer) + 1 : priority = 15: KnightLamp(CurrentPlayer) = KnightLamp(CurrentPlayer) + 1 End If'(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)

'	If KnightLamp(CurrentPlayer) = 1 Then pupevent 563 : KnightLamp(CurrentPlayer) = KnightLamp(CurrentPlayer) + 1 End If'(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	If KnightLamp(CurrentPlayer) = 3 Then pupevent 564 : KnightLamp(CurrentPlayer) = KnightLamp(CurrentPlayer) + 1 End If'(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	If KnightLamp(CurrentPlayer) = 5 Then pupevent 565 : KnightLamp(CurrentPlayer) = KnightLamp(CurrentPlayer) + 1 End If'(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	If KnightLamp(CurrentPlayer) = 7 Then pupevent 566 : KnightLamp(CurrentPlayer) = KnightLamp(CurrentPlayer) + 1 End If'(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	If KnightLamp(CurrentPlayer) = 9 Then pupevent 567 : KnightLamp(CurrentPlayer) = KnightLamp(CurrentPlayer) + 1 End If'(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	If KnightLamp(CurrentPlayer) = 11 Then pupevent 568 : KnightLamp(CurrentPlayer) = KnightLamp(CurrentPlayer) + 1 End If'(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
End Sub 

Sub Video_Catapult_Multball_1
	BypassVideo = False
	HideOverlay = True
	VideoHideOverlay
	EndOfVideo.Interval=4000
	EndOfVideo.Enabled=True
	If LastChanceCatapultMode = False Then
		Select Case Int(Rnd*2)+1
	'		Case 1 : pupevent 520
	'		Case 2 : pupevent 521
			Case 1 : VideoSelected = "BlackKnight_CatapultMultiball_AttackVariations_1.mp4"
			Case 2 : VideoSelected = "BlackKnight_CatapultMultiball_AttackVariations_2.mp4"
		End Select
	Else
		Select Case Int(Rnd*2)+1
	'		Case 1 : pupevent 520
	'		Case 2 : pupevent 521
			Case 1 : VideoSelected = "BlackKnight_SuperCatapultMultiball_AttackVariations_1.mp4"
			Case 2 : VideoSelected = "BlackKnight_SuperCatapultMultiball_AttackVariations_2.mp4"
		End Select
	End If
	playmedia VideoSelected,"Catapult Multiball",pBackglass,"cineon",4000,"",1,14  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	priority = 14
'	puPlayer.LabelSet pBackglass,"CommentDisplayed","Catapult Jackpot!",1,""
'	puPlayer.LabelSet pBackglass,"CommentDisplayed2","Catapult Jackpot!",1,""
'	puPlayer.LabelSet pBackglass,"AddScoreDisplayed","" & FormatNumber(AddscoreCatapultBonus,0),1,""
'	puPlayer.LabelSet pBackglass,"AddScoreDisplayed2","" & FormatNumber(AddscoreCatapultBonus,0),1,""
	If priority <= 14 Then
		pSplashCommentDisplayed "" & CommentDisplayed,3,255
		pSplashCommentDisplayed2 "" & CommentDisplayed,3,40
		pSplashAddScoreDisplayed "" & FormatNumber(AddscoreCatapultBonus,0),3,255
		pSplashAddScoreDisplayed2 "" & FormatNumber(AddscoreCatapultBonus,0),3,40
	End If
End Sub

'Sub Video_Catapult_Multball_2
'	HideOverlay = True
'	VideoHideOverlay
'	EndOfVideo.Interval=4000
'	EndOfVideo.Enabled=True
'	playmedia "BlackKnight_CatapultMultiball_AttackVariations_2.mp4","Catapult Multiball",pBackglass,"cineon",4000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
''	pupevent 521
'End Sub

Sub Video_Catapult_Multball_jacpot
	BypassVideo = False
	HideOverlay = True
	VideoHideOverlay
	EndOfVideo.Interval=4000
	EndOfVideo.Enabled=True
	playmedia "BlackKnight_CatapultMultiball_SuperJackpot.mp4","Catapult Multiball",pBackglass,"cineon",4000,"",1,16  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	priority = 16
'	puPlayer.LabelSet pBackglass,"CommentDisplayed","" & CommentDisplayed,1,""
'	puPlayer.LabelSet pBackglass,"CommentDisplayed2","" & CommentDisplayed,1,""
'	puPlayer.LabelSet pBackglass,"AddScoreDisplayed","" & FormatNumber(Catapult_jackpot_collected,0),1,""
'	puPlayer.LabelSet pBackglass,"AddScoreDisplayed2","" & FormatNumber(Catapult_jackpot_collected,0),1,""
	If priority <= 16 Then
		pSplashCommentDisplayed "" & CommentDisplayed,3,255
		pSplashCommentDisplayed2 "" & CommentDisplayed,3,40
		pSplashAddScoreDisplayed "" & FormatNumber(Catapult_jackpot_collected,0),3,255
		pSplashAddScoreDisplayed2 "" & FormatNumber(Catapult_jackpot_collected,0),3,40
	End If
	Catapult_jackpot_collected = 0
'	pupevent 524
End Sub

Sub Video_Add_Ball_Ready
	BypassVideo = False
	HideOverlay = True
	VideoHideOverlay
	EndOfVideo.Interval=4000
	EndOfVideo.Enabled=True
	playmedia "AddABallReady.mp4","Add a ball",pBackglass,"cineon",4000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	pupevent 501
End Sub

Sub Video_Add_Ball
	BypassVideo = False
	HideOverlay = True
	VideoHideOverlay
	EndOfVideo.Interval=4000
	EndOfVideo.Enabled=True
	playmedia "AddABall.mp4","Add a ball",pBackglass,"cineon",4000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	pupevent 500

'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
pupevent 500
'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
End Sub

Sub Video_Danger
	BypassVideo = False
	HideOverlay = True
	VideoHideOverlay
	EndOfVideo.Interval=4000
	EndOfVideo.Enabled=True
	'PuPlayer.PlayStop pBackglass
	playmedia "Danger.mp4","Danger",pBackglass,"cineon",4000,"",1,70 '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	priority = 70
'	pupevent 527
End Sub

Sub Video_Tilt
	BypassVideo = False
	HideOverlay = True
	VideoHideOverlay
	EndOfVideo.Interval=7400
	EndOfVideo.Enabled=True
	'PuPlayer.PlayStop pBackglass
	playmedia "Tilt.mp4","Tilt",pBackglass,"cineon",7400,"",1,71  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	priority = 71
'	pupevent 588
End Sub

Sub Video_Magna_Save
	BypassVideo = False
	HideOverlay = True
	VideoHideOverlay
	EndOfVideo.Interval=4000
	EndOfVideo.Enabled=True
	playmedia "BK_MagnaSave.mp4","Magna Save",pBackglass,"cineon",4000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	DOF 202, DOFPulse
'	pupevent 574
End Sub

Sub Video_Black_Knight_Retro
	BypassVideo = False
	HideOverlay = True
	VideoHideOverlay
	If BlackKnightRetro(CurrentPlayer) = 1 Then
		playmedia "WizardModeBKOG_Intro.mp4","BK2000",pBackglass,"cineon",5000,"",1,21  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		priority = 21
		'pupevent 509
		EndOfVideo.Interval=5000
		EndOfVideo.Enabled=True
		RetroMode = 1
		StartBlackKnightRetroMulti.Interval=5000
		StartBlackKnightRetroMulti.Enabled=True
	End If
	If BlackKnightRetro(CurrentPlayer) = 3 Then
		playmedia "WizardModeBK2K_Intro.mp4","BK2000",pBackglass,"cineon",4200,"",1,21  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		priority = 21
		'pupevent 507
		'EndOfVideo.Interval=4200
		'EndOfVideo.Enabled=True
		RetroMode = 3
		'StartBlackKnightRetroMulti.Interval=4200
		'StartBlackKnightRetroMulti.Enabled=True
		'horloge.Enabled=True
		BK2000Bonus.Interval=5000
		BK2000Bonus.Enabled=True
	End If
End Sub

Sub BK2000Bonus_Timer()
	BypassVideo = False
	BK2000Bonus.Enabled=False
	playmedia "BK2000 - Bonus.mp4","BK2000",pBackglass,"cineon",22000,"",1,21  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	priority = 22
	'pupevent 507
	EndOfVideo.Interval=22000
	EndOfVideo.Enabled=True
	RetroMode = 3
	StartBlackKnightRetroMulti.Interval=22000
	StartBlackKnightRetroMulti.Enabled=True
'	horloge.Enabled=True
	GiOff
	TurnOffRoundLights
	bk2000_01.Interval=6100
	bk2000_01.Enabled=True
	bk2000_02.Interval=8280
	bk2000_02.Enabled=True
	bk2000_03.Interval=11100
	bk2000_03.Enabled=True
	bk2000_04.Interval=14600
	bk2000_04.Enabled=True
	bk2000_05.Interval=16260
	bk2000_05.Enabled=True
	bk2000_06.Interval=19040
	bk2000_06.Enabled=True
	bk2000_07.Interval=22040
	bk2000_07.Enabled=True		

	Light054.State=2
	Light055.State=2
	Light056.State=2
	Light057.State=2
	Light058.State=2
	Light059.State=2
	Light060.State=2
	Light061.State=2
	Light062.State=2
	Light063.State=2
	L163.State = 2

	
	L83.State = 0
	L82.State = 0
	L79.State = 0
	L75.State = 0
	L106.State = 0
	L115.State = 0
	L116.State = 0
	L117.State = 0
End Sub


Sub bk2000_01_Timer()
	bk2000_01.Enabled=False
	CheckLamp.Enabled = False
	TurnOffAllRegularLights
	Light054.State=0
	Light055.State=0
	Light056.State=0
	Light057.State=0
	Light058.State=0
	Light059.State=0
	Light060.State=0
	Light061.State=0
	Light062.State=0
	Light063.State=0
	L163.State = 0
End Sub
Sub bk2000_02_Timer()
	bk2000_02.Enabled=False
	L83.State = 2
	L82.State = 2
	L79.State = 2
	StopBouleFlag = False
	StopRotating.Enabled=False
	timer2.interval=8
	timer2.Enabled=True
End Sub
Sub bk2000_03_Timer()
	bk2000_03.Enabled=False
	L83.State = 0
	L82.State = 0
	L79.State = 0
	L75.State = 2
	L106.State = 2
End Sub
Sub bk2000_04_Timer()
	bk2000_04.Enabled=False
	L115.State = 2
	L116.State = 2
	L117.State = 2
	L75.State = 0
	L106.State = 0
End Sub
Sub bk2000_05_Timer()
	bk2000_05.Enabled=False
	L115.State = 0
	L116.State = 0
	L117.State = 0
	L95.State = 2
End Sub
Sub bk2000_06_Timer()
	bk2000_06.Enabled=False
	ExtraBallsIsLit = True
	CheckLamp.Enabled = True
End Sub
Sub bk2000_07_Timer()
	bk2000_07.Enabled=False
	GiOn
End Sub


Sub EndOfVideo_Timer()
	priority = 0
	BypassVideo = False
	EndOfVideo.Enabled=False
	HideOverlay = False
	restoreOverlay.Interval=100
	restoreOverlay.Enabled=True
	GiOn
'''	SetBackGroundGeneral
	puPlayer.LabelSet pBackglass,"CommentDisplayed","",1,""
	puPlayer.LabelSet pBackglass,"CommentDisplayed2","",1,""
	puPlayer.LabelSet pBackglass,"AddScoreDisplayed","",1,""
	puPlayer.LabelSet pBackglass,"AddScoreDisplayed2","",1,""
End Sub

Sub EndOfMode_Timer()
	VideoIsPlayed = False
	BypassVideo = False
	EndOfMode.Enabled=False
	HideOverlay = False
	restoreOverlay.Interval=100
	restoreOverlay.Enabled=True
	GiOn
	RestoreVideoSetBackground.Interval = 1000
	RestoreVideoSetBackground.Enabled=True
End Sub

Sub Endvideo_fire_timer()
	FireAddScore.Enabled=False
	Endvideo_fire.Enabled=False
	GiOn
End Sub

Sub EndOfVideoLastChance_Timer()
	HideOverlay = False
	EndOfVideoLastChance.Enabled = False
	AddTimeForMission(CurrentPlayer) = MissionTimeLastChance
	LastChanceStart = True
	AddscoreCatapult = 250000
	restoreOverlay.Interval=100
	restoreOverlay.Enabled=True
End Sub

Sub EndOfVideoGameOver_Timer()
	BypassVideo = False
	EndOfVideoGameOver.Enabled=False
	HideOverlay = True
	VideoHideOverlay
	StartAttractMode		'after the end of MatchingScore
'	video_init2
	InProgress= False
	BallsRemaining(1)=3

End Sub

Sub RestoreVideoSetBackground_Timer()
	RestoreVideoSetBackground.Enabled=False
	VideoSetBackground
	MusicCheck 22
End Sub

Sub restoreOverlay_timer
'	BypassVideo = False
'	If BallsOnPlayfield <> 0 And Drainer.Enabled = True Then
	If HideTotalBonusScore.Enabled = False Then
		restoreOverlay.enabled=False
		'	UpdateSuperOverlay

		compteuri = compteuri + 1
		UpdateSuperOverlayCount = UpdateSuperOverlayCount + 1
'--------->'29,35,41,47,53,59
'			L29State = 1
		If SuperFeaturesColor = "Yellow" Then
			SuperFeaturesColorSelected = "Y"
			SuperFeaturesColorNumber = "3"
		Elseif SuperFeaturesColor = "Red" Then
			SuperFeaturesColorSelected = "R"
			SuperFeaturesColorNumber = "4"
		Elseif SuperFeaturesColor = "Magenta" Then
			SuperFeaturesColorSelected = "M"
			SuperFeaturesColorNumber = "5"
		Elseif SuperFeaturesColor = "Green" Then
			SuperFeaturesColorSelected = "G"
			SuperFeaturesColorNumber = "2"
		Else 
			SuperFeaturesColorSelected = "Y"
			SuperFeaturesColorNumber = "3"
		End If

		If SuperTargets = True Then
			SuperSelectCalcul = SuperSelectCalcul + SuperFeaturesColorNumber
			L29State = 1
			If SuperFeaturesColorNumber = 2 Then AddScoreSuperTargets = 100000 End If
			If SuperFeaturesColorNumber = 3 Then AddScoreSuperTargets = 150000 End If
			If SuperFeaturesColorNumber = 4 Then AddScoreSuperTargets = 250000 End If
			If SuperFeaturesColorNumber = 5 Then AddScoreSuperTargets = 350000 End If
		Else
			SuperSelectCalcul = SuperSelectCalcul + 1
		End If
		If SuperSlings = True Then
			SuperSelectCalcul = SuperSelectCalcul + (SuperFeaturesColorNumber * 10)
			L59State = 1
			If SuperFeaturesColorNumber = 2 Then AddScoreSuperSlings = 100000 End If
			If SuperFeaturesColorNumber = 3 Then AddScoreSuperSlings = 200000 End If
			If SuperFeaturesColorNumber = 4 Then AddScoreSuperSlings = 350000 End If
			If SuperFeaturesColorNumber = 5 Then AddScoreSuperSlings = 500000 End If
		Else
			SuperSelectCalcul = SuperSelectCalcul + 10
		End If
		If SuperLanes = True Then
			SuperSelectCalcul = SuperSelectCalcul + (SuperFeaturesColorNumber * 100)
			L53State = 1
			If SuperFeaturesColorNumber = 2 Then AddScoreSuperLanes = 50000 End If
			If SuperFeaturesColorNumber = 3 Then AddScoreSuperLanes = 75000 End If
			If SuperFeaturesColorNumber = 4 Then AddScoreSuperLanes = 115000 End If
			If SuperFeaturesColorNumber = 5 Then AddScoreSuperLanes = 140000 End If
		Else
			SuperSelectCalcul = SuperSelectCalcul + 100
		End If
		If SuperPops = True Then
			SuperSelectCalcul = SuperSelectCalcul + (SuperFeaturesColorNumber * 1000)
			L41State = 1
			If SuperFeaturesColorNumber = 2 Then AddScoreSuperPops = 35000 End If
			If SuperFeaturesColorNumber = 3 Then AddScoreSuperPops = 50000 End If
			If SuperFeaturesColorNumber = 4 Then AddScoreSuperPops = 95000 End If
			If SuperFeaturesColorNumber = 5 Then AddScoreSuperPops = 115000 End If
		Else
			SuperSelectCalcul = SuperSelectCalcul + 1000
		End If
		If SuperOrbits = True Then
			SuperSelectCalcul = SuperSelectCalcul + (SuperFeaturesColorNumber * 10000)
			L47State = 1
			If SuperFeaturesColorNumber = 2 Then AddScoreSuperOrbits = 115000 End If
			If SuperFeaturesColorNumber = 3 Then AddScoreSuperOrbits = 185000 End If
			If SuperFeaturesColorNumber = 4 Then AddScoreSuperOrbits = 275000 End If
			If SuperFeaturesColorNumber = 5 Then AddScoreSuperOrbits = 400000 End If
		Else
			SuperSelectCalcul = SuperSelectCalcul + 10000
		End If
		If SuperSpinner = True Then
			SuperSelectCalcul = SuperSelectCalcul + (SuperFeaturesColorNumber * 100000)
			L35State = 1
			If SuperFeaturesColorNumber = 2 Then AddScoreSuperSpinner = 14000 End If
			If SuperFeaturesColorNumber = 3 Then AddScoreSuperSpinner = 20000 End If
			If SuperFeaturesColorNumber = 4 Then AddScoreSuperSpinner = 28000 End If
			If SuperFeaturesColorNumber = 5 Then AddScoreSuperSpinner = 38000 End If
		Else
			SuperSelectCalcul = SuperSelectCalcul + 100000
		End If
'		SuperOverlaySelected = "Suoer"&SuperSelectCalcul&".png"
'		playmedia SuperOverlaySelected,"PuPOverlays",pBackGlass2,"cineon",100,"",1,compteuri '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)

	'If HideOverlay = False And OldstateHideOverlay <> HideOverlay Then
	If HideOverlay = False Then
'		UpdateSuperOverlay
		OverlaySelected = "overlay_blank.png"
'		Overlaydirectory = "PuPOverlays"
		If NewRecord = True Then
			OverlaySelected = "EnterIntials.png"
			'playmedia "EnterIntials.png","PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		ElseIf PlayersPlayingGame = 1 And RetroMode <> 1 And RetroMode <> 3 And LastChanceStart = False Then
			If RetroMode = 0 Then 
'				OverlaySelected = "overlay.png"
'				SuperOverlaySelected = "Suoer"&SuperSelectCalcul&".png"
				OverlaySelected = "P1 "&SuperFeaturesColorSelected&" "&SuperSelectCalcul&".png"
				playmedia OverlaySelected,"PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'				Overlaydirectory = "PuPOverlays P1"
'				pupevent 611
			End If
'		End If
		ElseIf PlayersPlayingGame = 2 And RetroMode <> 1 And RetroMode <> 3 And LastChanceStart = False Then
			If RetroMode = 0 Then 
'				playmedia OverlaySelected,"PuPOverlaysP2",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				OverlaySelected = "P2 "&SuperFeaturesColorSelected&" "&SuperSelectCalcul&".png"
				playmedia OverlaySelected,"PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'				Overlaydirectory = "PuPOverlays P2"
'				pupevent 612
			End If
'		End If
		ElseIf PlayersPlayingGame = 3 And RetroMode <> 1 And RetroMode <> 3 And LastChanceStart = False Then
			If RetroMode = 0 Then 
'				playmedia OverlaySelected,"PuPOverlaysP3",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				OverlaySelected = "P3 "&SuperFeaturesColorSelected&" "&SuperSelectCalcul&".png"
				playmedia OverlaySelected,"PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'				Overlaydirectory = "PuPOverlays P3"
'				pupevent 613
			End If
'		End If
		ElseIf PlayersPlayingGame = 4 And RetroMode <> 1 And RetroMode <> 3 And LastChanceStart = False Then
			If RetroMode = 0 Then 
'				playmedia "overlay4p.png","PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				OverlaySelected = "P4 "&SuperFeaturesColorSelected&" "&SuperSelectCalcul&".png"
				playmedia OverlaySelected,"PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'				Overlaydirectory = "PuPOverlays P4"
'				pupevent 614 
			End If
'		End If
		'If BlackKnightRetro(CurrentPlayer) = 1 Then
		ElseIf RetroMode = 1 Then
			playmedia "bkretro.png","PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			OverlaySelected = "bkretro.png"
'			Overlaydirectory = "PuPOverlays"
'				pupevent 616
'		End If
		'If BlackKnightRetro(CurrentPlayer) = 3 Then
		ElseIf RetroMode = 3 Then
			playmedia "bk2000retro.png","PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			OverlaySelected = "bk2000retro.png"
'			Overlaydirectory = "PuPOverlays"
'				pupevent 617
'		End If
		ElseIf LastChanceStart = True Then
			playmedia "overlay_LastChance.png","PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			OverlaySelected = "overlay_LastChance.png"
'			Overlaydirectory = "PuPOverlays"
			LastChanceBallToLock = 3 - BallInCatapult(CurrentPlayer)
			puPlayer.LabelSet pBackglass,"LastChanceBallToLock","Lock "& FormatNumber(LastChanceBallToLock,0)&" Balls",1,""
			puPlayer.LabelSet pBackglass,"LastChanceTime",""& FormatNumber(AddTimeForMission(CurrentPlayer),0)&" ",1,""
			puPlayer.LabelSet pBackglass,"LastChanceJackpot","Jackpots : "& FormatNumber(AddscoreCatapult,0)&" ",1,""	
		End If

		playmedia OverlaySelected,"PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	End If
	OldstateHideOverlay = HideOverlay
'	UpdateSuperOverlay
'	TimeSuperMode.Enabled = True



	SuperSelectCalcul = 0
	End If
End Sub

'*************************************************************************************************************************************
'*************************************************************************************************************************************
'*************************************************************************************************************************************
'********************************************        NEW PUP PLAYER  in TEST...     **************************************************
'*************************************************************************************************************************************
'*************************************************************************************************************************************
'*************************************************************************************************************************************

'DMD "                    ", "                    ", "", eNone, eNone, eNone, 1000, True, ""
' COPY EVERYTHING BELOW TO THE TOP OF YOUR TABLE SCRIPT UNDER OPTION EXPLICIT                             Start Pup Pack

'****** PuP Variables ******

Dim usePUP: Dim cPuPPack: Dim PUPStatus: PUPStatus=False ' dont edit this line!!!
'Dim PuPlayer :

'*************************** PuP Settings for this table ********************************

usePUP   = true               ' enable Pinup Player functions for this table
cPuPPack = "bksor"    ' name of the PuP-Pack / PuPVideos folder for this table

'//////////////////// PINUP PLAYER: STARTUP & CONTROL SECTION //////////////////////////

' This is used for the startup and control of Pinup Player

Sub PuPStart(cPuPPack)
    If PUPStatus=true then Exit Sub
    If usePUP=true then
        Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay")
        If PuPlayer is Nothing Then
            usePUP=false
            PUPStatus=false
        Else
            PuPlayer.B2SInit "",cPuPPack 'start the Pup-Pack
            PUPStatus=true
        End If
    End If
End Sub

Sub pupevent(EventNum)
'	If SetBackGroundImportant = True Then
'		If EndOfVideo.Enabled = True Then 
'			EventNumold = EventNum
'			PupeventRecall.Enabled = True
'		Else
'			If EventNumold <> "" Then
'				SetBackGroundImportant = False
'				EventNum = EventNumold
'				EventNumold = ""
'			End If
'		End If
'	End If

	if (usePUP=false or PUPStatus=false) then Exit Sub
	PuPlayer.B2SData "E"&EventNum,1  'send event to Pup-Pack
End Sub

Sub PupeventRecall_timer()
	PupeventRecall.Enabled = False
	pupevent EventNumold
	irecall = irecall + 1
End Sub

' ******* How to use PUPEvent to trigger / control a PuP-Pack *******

' Usage: pupevent(EventNum)

' EventNum = PuP Exxx trigger from the PuP-Pack

' Example: pupevent 102

' This will trigger E102 from the table's PuP-Pack

' DO NOT use any Exxx triggers already used for DOF (if used) to avoid any possible confusion

'************ PuP-Pack Startup **************

PuPStart(cPuPPack) 'Check for PuP - If found, then start Pinup Player / PuP-Pack


'********************
'Light definitions
'********************

LightType(1) = 0 : Set LightObjectColor(1) = L26 'MUD BOG
LightType(2) = 0 : Set LightObjectColor(2) = L29 'SUPER TARGETS
LightType(3) = 0 : Set LightObjectColor(3) = L32 'MOLTEN FIRE
LightType(4) = 0 : Set LightObjectColor(4) = L35 'SUPER SPINS
LightType(5) = 0 : Set LightObjectColor(5) = L38 'BURNING SANDS
LightType(6) = 0 : Set LightObjectColor(6) = L41 'SUPER POPS
LightType(7) = 0 : Set LightObjectColor(7) = L44 'WICKED CAVERN
LightType(8) = 0 : Set LightObjectColor(8) = L47 'SUPER ORBITS
LightType(9) = 0 : Set LightObjectColor(9) = L50 'DEEP FREEZE
LightType(10) = 0 : Set LightObjectColor(10) = L53 'SUPER LANES
LightType(11) = 0 : Set LightObjectColor(11) = L56 'BLACK CASTLE
LightType(12) = 0 : Set LightObjectColor(12) = L59 'SUPER SLINGS

'***************************************************
'    Lamps & Flashers 
'***************************************************



Sub LampRoundAnimation_Timer()
	Dim chgLamp, ii
    'chgLamp = Controller.ChangedLamps
	If lightstateflag = 1 Then	
		LightObjectColor(Lightcounter).FadeSpeedDown = LightObjectColor(Lightcounter).Intensity /  80000
		LightObjectColor(Lightcounter).state = 0
		lightstateflag = 0
		Lightcounter = Lightcounter + 1
		'LampRoundAnimation.interval= 200
	Else	
		LightObjectColor(Lightcounter).Intensity= 500
		LightObjectColor(Lightcounter).state = 1
'		Lightobjectsimplify(1).state = 1
		lightstateflag = 1
		'LampRoundAnimation.interval= 200
	End If
	If Lightcounter = 13 Then
		Lightcounter = 1
		LampRoundAnimation.Enabled=False
		'ColorRoundLight = 1
		animation_start_and_finish_flag = 1	
	End If
End Sub

Sub LampRoundAnimation2_Timer()	
	If Lightcounter2 = 1 Then
		TurnOffRoundLights
	End If
	If lightstateflag = 1 Then	
		lightstateflag = 0
		LightObjectColor(1).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(1).state = 0
		LightObjectColor(2).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(2).state = 2
		LightObjectColor(3).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(3).state = 0
		LightObjectColor(4).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(4).state = 2
		LightObjectColor(5).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(5).state = 0
		LightObjectColor(6).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(6).state = 2
		LightObjectColor(7).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(7).state = 0
		LightObjectColor(8).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(8).state = 2
		LightObjectColor(9).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(9).state = 0
		LightObjectColor(10).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(10).state = 2
		LightObjectColor(11).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(11).state = 0
		LightObjectColor(12).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(12).state = 2
		Lightcounter2 = Lightcounter2 + 1
		'LampRoundAnimation2.interval= 200
'		Lightobjectsimplify(1).state = 2
	Else	
		lightstateflag = 1
		LightObjectColor(1).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(1).state = 2
		LightObjectColor(2).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(2).state = 0
		LightObjectColor(3).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(3).state = 2
		LightObjectColor(4).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(4).state = 0
		LightObjectColor(5).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(5).state = 2
		LightObjectColor(6).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(6).state = 0
		LightObjectColor(7).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(7).state = 2
		LightObjectColor(8).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(8).state = 0
		LightObjectColor(9).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(9).state = 2
		LightObjectColor(10).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(10).state = 0
		LightObjectColor(11).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(11).state = 2
		LightObjectColor(12).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(12).state = 0
		'LampRoundAnimation2.interval= 200
	End If
	If Lightcounter2 = 13 Then
		Lightcounter2 = 1
		LampRoundAnimation2.Enabled=False
		'ColorRoundLight = 1
		TurnOffRoundLights
		animation_start_and_finish_flag = 1
	End If
End Sub
Sub LampRoundAnimation3_Timer()	
	If Lightcounter2 = 1 Then
		TurnOffRoundLights
	End If
	If lightstateflag = 1 Then	
		lightstateflag = 0
		LightObjectColor(1).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(1).state = 2
		LightObjectColor(2).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(2).state = 2
		LightObjectColor(3).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(3).state = 2
		LightObjectColor(4).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(4).state = 2
		LightObjectColor(5).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(5).state = 2
		LightObjectColor(6).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(6).state = 2
		LightObjectColor(7).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(7).state = 2
		LightObjectColor(8).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(8).state = 2
		LightObjectColor(9).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(9).state = 2
		LightObjectColor(10).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(10).state = 2
		LightObjectColor(11).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(11).state = 2
		LightObjectColor(12).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(12).state = 2
		Lightcounter2 = Lightcounter2 + 1
	Else	
		lightstateflag = 1
		TurnOffRoundLights
	End If
	If Lightcounter2 = 13 Then
		Lightcounter2 = 1
		LampRoundAnimation3.Enabled=False
		'ColorRoundLight = 1
		TurnOffRoundLights
		animation_start_and_finish_flag = 1
	End If
End Sub

Sub LampRoundAnimation4_Timer()	
	If Lightcounter2 = 1 Then
		TurnOffRoundLights
		'lightstateflag = 0
	End If
	If Lightcounter2 = 8 Then
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 7 Then
		LightObjectColor(12).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(12).state = 1
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 6 Then
		LightObjectColor(1).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(1).state = 1
		LightObjectColor(11).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(11).state = 1
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 5 Then
		LightObjectColor(2).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(2).state = 1
		LightObjectColor(10).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(10).state = 1
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 4 Then
		LightObjectColor(3).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(3).state = 1
		LightObjectColor(9).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(9).state = 1
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 3 Then
		LightObjectColor(4).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(4).state = 1
		LightObjectColor(8).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(8).state = 1
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 2 Then
		LightObjectColor(5).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(5).state = 1
		LightObjectColor(7).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(7).state = 1
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 1 Then	
		LightObjectColor(6).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(6).state = 1
		Lightcounter2 = Lightcounter2 + 1
	End If
	If LightCounter2 = 9 Then
		Lightcounter2 = 1
		LampRoundAnimation4.Enabled=False
		TurnOffRoundLights
		animation_start_and_finish_flag = 1
	End If	
End Sub

Sub LampRoundAnimation5_Timer()	
	If Lightcounter2 = 1 Then
		TurnOffRoundLights
		'lightstateflag = 0
	End If
	If Lightcounter2 = 35 Then
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 34 Then
		LightObjectColor(1).state = 1
		LightObjectColor(2).state = 1
		LightObjectColor(3).state = 1
		LightObjectColor(4).state = 1
		LightObjectColor(5).state = 1
		LightObjectColor(6).state = 1
		LightObjectColor(7).state = 1
		LightObjectColor(8).state = 1
		LightObjectColor(9).state = 1
		LightObjectColor(10).state = 1
		LightObjectColor(11).state = 1
		LightObjectColor(12).state = 1
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 33 Then
		LightObjectColor(8).state = 1
		LightObjectColor(9).state = 1
		LightObjectColor(10).state = 1
		LightObjectColor(11).state = 1
		LightObjectColor(12).state = 1
		LightObjectColor(2).state = 1
		LightObjectColor(3).state = 1
		LightObjectColor(4).state = 1
		LightObjectColor(5).state = 1
		LightObjectColor(6).state = 1
		LightObjectColor(1).state = 0
		LightObjectColor(7).state = 0
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 32 Then
		LightObjectColor(9).state = 1
		LightObjectColor(10).state = 1
		LightObjectColor(11).state = 1
		LightObjectColor(12).state = 1
		LightObjectColor(1).state = 1
		LightObjectColor(3).state = 1
		LightObjectColor(4).state = 1
		LightObjectColor(5).state = 1
		LightObjectColor(6).state = 1
		LightObjectColor(7).state = 1
		LightObjectColor(2).state = 0
		LightObjectColor(8).state = 0
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 31 Then
		LightObjectColor(10).state = 1
		LightObjectColor(11).state = 1
		LightObjectColor(12).state = 1
		LightObjectColor(1).state = 1
		LightObjectColor(2).state = 1
		LightObjectColor(4).state = 1
		LightObjectColor(5).state = 1
		LightObjectColor(6).state = 1
		LightObjectColor(7).state = 1
		LightObjectColor(8).state = 1
		LightObjectColor(3).state = 0
		LightObjectColor(9).state = 0
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 30 Then
		LightObjectColor(12).state = 1
		LightObjectColor(1).state = 1
		LightObjectColor(2).state = 1
		LightObjectColor(3).state = 1
		LightObjectColor(6).state = 1
		LightObjectColor(7).state = 1
		LightObjectColor(8).state = 1
		LightObjectColor(9).state = 1
		LightObjectColor(4).state = 0
		LightObjectColor(5).state = 0
		LightObjectColor(10).state = 0
		LightObjectColor(11).state = 0
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 29 Then
		LightObjectColor(1).state = 1
		LightObjectColor(2).state = 1
		LightObjectColor(3).state = 1
		LightObjectColor(4).state = 1
		LightObjectColor(7).state = 1
		LightObjectColor(8).state = 1
		LightObjectColor(9).state = 1
		LightObjectColor(10).state = 1
		LightObjectColor(5).state = 0
		LightObjectColor(6).state = 0
		LightObjectColor(11).state = 0
		LightObjectColor(12).state = 0
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 28 Then
		LightObjectColor(2).state = 1
		LightObjectColor(3).state = 1
		LightObjectColor(4).state = 1
		LightObjectColor(5).state = 1
		LightObjectColor(8).state = 1
		LightObjectColor(9).state = 1
		LightObjectColor(10).state = 1
		LightObjectColor(11).state = 1
		LightObjectColor(1).state = 0
		LightObjectColor(6).state = 0
		LightObjectColor(7).state = 0
		LightObjectColor(12).state = 0
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 27 Then
		LightObjectColor(3).state = 1
		LightObjectColor(4).state = 1
		LightObjectColor(5).state = 1
		LightObjectColor(6).state = 1
		LightObjectColor(10).state = 1
		LightObjectColor(11).state = 1
		LightObjectColor(12).state = 1
		LightObjectColor(1).state = 0
		LightObjectColor(2).state = 0
		LightObjectColor(7).state = 0
		LightObjectColor(8).state = 0
		LightObjectColor(9).state = 0
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 26 Then
		LightObjectColor(5).state = 1
		LightObjectColor(6).state = 1
		LightObjectColor(7).state = 1
		LightObjectColor(11).state = 1
		LightObjectColor(12).state = 1
		LightObjectColor(1).state = 1
		LightObjectColor(2).state = 0
		LightObjectColor(3).state = 0
		LightObjectColor(4).state = 0
		LightObjectColor(8).state = 0
		LightObjectColor(9).state = 0
		LightObjectColor(10).state = 0
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 25 Then
		LightObjectColor(7).state = 1
		LightObjectColor(8).state = 1
		LightObjectColor(9).state = 1
		LightObjectColor(12).state = 1
		LightObjectColor(1).state = 1
		LightObjectColor(2).state = 1
		LightObjectColor(3).state = 0
		LightObjectColor(4).state = 0
		LightObjectColor(5).state = 0
		LightObjectColor(6).state = 0
		LightObjectColor(10).state = 0
		LightObjectColor(11).state = 0
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 24 Then
		LightObjectColor(8).state = 1
		LightObjectColor(9).state = 1
		LightObjectColor(10).state = 1
		LightObjectColor(1).state = 1
		LightObjectColor(2).state = 1
		LightObjectColor(3).state = 1
		LightObjectColor(4).state = 0
		LightObjectColor(5).state = 0
		LightObjectColor(6).state = 0
		LightObjectColor(7).state = 0
		LightObjectColor(11).state = 0
		LightObjectColor(12).state = 0
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 23 Then
		LightObjectColor(9).state = 1
		LightObjectColor(10).state = 1
		LightObjectColor(11).state = 1
		LightObjectColor(2).state = 1
		LightObjectColor(3).state = 1
		LightObjectColor(4).state = 1
		LightObjectColor(1).state = 0
		LightObjectColor(5).state = 0
		LightObjectColor(6).state = 0
		LightObjectColor(7).state = 0
		LightObjectColor(8).state = 0
		LightObjectColor(12).state = 0
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 22 Then
		LightObjectColor(10).state = 1
		LightObjectColor(11).state = 1
		LightObjectColor(12).state = 1
		LightObjectColor(3).state = 1
		LightObjectColor(4).state = 1
		LightObjectColor(5).state = 1
		LightObjectColor(1).state = 0
		LightObjectColor(2).state = 0
		LightObjectColor(6).state = 0
		LightObjectColor(7).state = 0
		LightObjectColor(8).state = 0
		LightObjectColor(9).state = 0
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 21 Then
		LightObjectColor(11).state = 1
		LightObjectColor(12).state = 1
		LightObjectColor(1).state = 1
		LightObjectColor(4).state = 1
		LightObjectColor(6).state = 1
		LightObjectColor(7).state = 1
		LightObjectColor(2).state = 0
		LightObjectColor(3).state = 0
		LightObjectColor(5).state = 0
		LightObjectColor(8).state = 0
		LightObjectColor(9).state = 0
		LightObjectColor(10).state = 0
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 20 Then
		LightObjectColor(12).state = 1
		LightObjectColor(1).state = 1
		LightObjectColor(2).state = 1
		LightObjectColor(5).state = 1
		LightObjectColor(7).state = 1
		LightObjectColor(8).state = 1
		LightObjectColor(3).state = 0
		LightObjectColor(4).state = 0
		LightObjectColor(6).state = 0
		LightObjectColor(9).state = 0
		LightObjectColor(10).state = 0
		LightObjectColor(11).state = 0
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 19 Then
		LightObjectColor(6).state = 1
		LightObjectColor(8).state = 1
		LightObjectColor(9).state = 1
		LightObjectColor(1).state = 0
		LightObjectColor(2).state = 0
		LightObjectColor(3).state = 0
		LightObjectColor(4).state = 0
		LightObjectColor(5).state = 0
		LightObjectColor(7).state = 0
		LightObjectColor(10).state = 0
		LightObjectColor(11).state = 0
		LightObjectColor(12).state = 0
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 18 Then
		LightObjectColor(7).state = 1
		LightObjectColor(9).state = 1
		LightObjectColor(10).state = 1
		LightObjectColor(1).state = 0
		LightObjectColor(2).state = 0
		LightObjectColor(3).state = 0
		LightObjectColor(4).state = 0
		LightObjectColor(5).state = 0
		LightObjectColor(6).state = 0
		LightObjectColor(8).state = 0
		LightObjectColor(11).state = 0
		LightObjectColor(12).state = 0
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 17 Then
		LightObjectColor(8).state = 1		
		LightObjectColor(10).state = 1
		LightObjectColor(11).state = 1
		LightObjectColor(1).state = 0
		LightObjectColor(2).state = 0
		LightObjectColor(3).state = 0
		LightObjectColor(4).state = 0
		LightObjectColor(5).state = 0
		LightObjectColor(6).state = 0
		LightObjectColor(7).state = 0
		LightObjectColor(9).state = 0
		LightObjectColor(12).state = 0
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 16 Then
		LightObjectColor(9).state = 1
		LightObjectColor(11).state = 1
		LightObjectColor(12).state = 1
		LightObjectColor(1).state = 0
		LightObjectColor(2).state = 0
		LightObjectColor(3).state = 0
		LightObjectColor(4).state = 0
		LightObjectColor(5).state = 0
		LightObjectColor(6).state = 0
		LightObjectColor(7).state = 0
		LightObjectColor(8).state = 0
		LightObjectColor(10).state = 0

		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 15 Then
		LightObjectColor(10).state = 1
		LightObjectColor(1).state = 1
		LightObjectColor(2).state = 0
		LightObjectColor(3).state = 0
		LightObjectColor(4).state = 0
		LightObjectColor(5).state = 0
		LightObjectColor(6).state = 0
		LightObjectColor(7).state = 0
		LightObjectColor(8).state = 0
		LightObjectColor(9).state = 0
		LightObjectColor(11).state = 0
		LightObjectColor(12).state = 0
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 14 Then
		LightObjectColor(11).state = 1
		LightObjectColor(2).state = 1
		LightObjectColor(1).state = 0
		LightObjectColor(3).state = 0
		LightObjectColor(4).state = 0
		LightObjectColor(5).state = 0
		LightObjectColor(6).state = 0
		LightObjectColor(7).state = 0
		LightObjectColor(8).state = 0
		LightObjectColor(9).state = 0
		LightObjectColor(10).state = 0
		LightObjectColor(12).state = 0
		Lightcounter2 = Lightcounter2 + 1
	End If

	If Lightcounter2 = 13 Then
		LightObjectColor(12).state = 1
		LightObjectColor(3).state = 1
		LightObjectColor(1).state = 0
		LightObjectColor(2).state = 0
		LightObjectColor(4).state = 0
		LightObjectColor(5).state = 0
		LightObjectColor(6).state = 0
		LightObjectColor(7).state = 0
		LightObjectColor(8).state = 0
		LightObjectColor(9).state = 0
		LightObjectColor(10).state = 0
		LightObjectColor(11).state = 0
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 12 Then
		LightObjectColor(1).state = 1
		LightObjectColor(4).state = 1
		LightObjectColor(2).state = 0
		LightObjectColor(3).state = 0
		LightObjectColor(5).state = 0
		LightObjectColor(6).state = 0
		LightObjectColor(7).state = 0
		LightObjectColor(8).state = 0
		LightObjectColor(9).state = 0
		LightObjectColor(10).state = 0
		LightObjectColor(11).state = 0
		LightObjectColor(12).state = 0
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 11 Then
		LightObjectColor(2).state = 1
		LightObjectColor(5).state = 1
		LightObjectColor(1).state = 0
		LightObjectColor(3).state = 0
		LightObjectColor(4).state = 0
		LightObjectColor(6).state = 0
		LightObjectColor(7).state = 0
		LightObjectColor(8).state = 0
		LightObjectColor(9).state = 0
		LightObjectColor(10).state = 0
		LightObjectColor(11).state = 0
		LightObjectColor(12).state = 0
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 10 Then
		LightObjectColor(3).state = 1
		LightObjectColor(6).state = 1
		LightObjectColor(1).state = 0
		LightObjectColor(2).state = 0
		LightObjectColor(4).state = 0
		LightObjectColor(5).state = 0
		LightObjectColor(7).state = 0
		LightObjectColor(8).state = 0
		LightObjectColor(9).state = 0
		LightObjectColor(10).state = 0
		LightObjectColor(11).state = 0
		LightObjectColor(12).state = 0
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 9 Then
		LightObjectColor(4).state = 1
		LightObjectColor(7).state = 1
		LightObjectColor(1).state = 0
		LightObjectColor(2).state = 0
		LightObjectColor(3).state = 0
		LightObjectColor(5).state = 0
		LightObjectColor(6).state = 0
		LightObjectColor(8).state = 0
		LightObjectColor(9).state = 0
		LightObjectColor(10).state = 0
		LightObjectColor(11).state = 0
		LightObjectColor(12).state = 0
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 8 Then
		LightObjectColor(5).state = 1
		LightObjectColor(8).state = 1
		LightObjectColor(1).state = 0
		LightObjectColor(2).state = 0
		LightObjectColor(3).state = 0
		LightObjectColor(4).state = 0
		LightObjectColor(6).state = 0
		LightObjectColor(7).state = 0
		LightObjectColor(9).state = 0
		LightObjectColor(10).state = 0
		LightObjectColor(11).state = 0
		LightObjectColor(12).state = 0
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 7 Then
		LightObjectColor(6).state = 1
		LightObjectColor(9).state = 1
		LightObjectColor(1).state = 0
		LightObjectColor(2).state = 0
		LightObjectColor(3).state = 0
		LightObjectColor(4).state = 0
		LightObjectColor(5).state = 0
		LightObjectColor(7).state = 0
		LightObjectColor(8).state = 0
		LightObjectColor(10).state = 0
		LightObjectColor(11).state = 0
		LightObjectColor(12).state = 0
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 6 Then
		LightObjectColor(7).state = 1
		LightObjectColor(10).state = 1
				LightObjectColor(1).state = 0
		LightObjectColor(2).state = 0
		LightObjectColor(3).state = 0
		LightObjectColor(4).state = 0
		LightObjectColor(5).state = 0
		LightObjectColor(6).state = 0
		LightObjectColor(8).state = 0
		LightObjectColor(9).state = 0
		LightObjectColor(11).state = 0
		LightObjectColor(12).state = 0
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 5 Then
		LightObjectColor(8).state = 1
		LightObjectColor(11).state = 1
				LightObjectColor(1).state = 0
		LightObjectColor(2).state = 0
		LightObjectColor(3).state = 0
		LightObjectColor(4).state = 0
		LightObjectColor(5).state = 0
		LightObjectColor(6).state = 0
		LightObjectColor(7).state = 0
		LightObjectColor(9).state = 0
		LightObjectColor(10).state = 0
		LightObjectColor(12).state = 0
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 4 Then
		LightObjectColor(9).state = 1
		LightObjectColor(12).state = 1
		LightObjectColor(1).state = 0
		LightObjectColor(2).state = 0
		LightObjectColor(3).state = 0
		LightObjectColor(4).state = 0
		LightObjectColor(5).state = 0
		LightObjectColor(6).state = 0
		LightObjectColor(7).state = 0
		LightObjectColor(8).state = 0
		LightObjectColor(10).state = 0
		LightObjectColor(11).state = 0
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 3 Then
		LightObjectColor(10).state = 1
		LightObjectColor(1).state = 0
		LightObjectColor(2).state = 0
		LightObjectColor(3).state = 0
		LightObjectColor(4).state = 0
		LightObjectColor(5).state = 0
		LightObjectColor(6).state = 0
		LightObjectColor(7).state = 0
		LightObjectColor(8).state = 0
		LightObjectColor(9).state = 0
		LightObjectColor(11).state = 0
		LightObjectColor(12).state = 0
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 2 Then
		LightObjectColor(11).state = 1
		LightObjectColor(1).state = 0
		LightObjectColor(2).state = 0
		LightObjectColor(3).state = 0
		LightObjectColor(4).state = 0
		LightObjectColor(5).state = 0
		LightObjectColor(6).state = 0
		LightObjectColor(7).state = 0
		LightObjectColor(8).state = 0
		LightObjectColor(9).state = 0
		LightObjectColor(10).state = 0
		LightObjectColor(12).state = 0
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 1 Then	
		LightObjectColor(12).state = 1
		LightObjectColor(1).state = 0
		LightObjectColor(2).state = 0
		LightObjectColor(3).state = 0
		LightObjectColor(4).state = 0
		LightObjectColor(5).state = 0
		LightObjectColor(6).state = 0
		LightObjectColor(7).state = 0
		LightObjectColor(8).state = 0
		LightObjectColor(9).state = 0
		LightObjectColor(10).state = 0
		LightObjectColor(11).state = 0
		LightObjectColor(1).FadeSpeedDown = LightObjectColor(1).Intensity /  1
		LightObjectColor(2).FadeSpeedDown = LightObjectColor(1).Intensity /  1
		LightObjectColor(3).FadeSpeedDown = LightObjectColor(1).Intensity /  1
		LightObjectColor(4).FadeSpeedDown = LightObjectColor(1).Intensity /  1
		LightObjectColor(5).FadeSpeedDown = LightObjectColor(1).Intensity /  1
		LightObjectColor(6).FadeSpeedDown = LightObjectColor(1).Intensity /  1
		LightObjectColor(7).FadeSpeedDown = LightObjectColor(1).Intensity /  1		
		LightObjectColor(8).FadeSpeedDown = LightObjectColor(1).Intensity /  1
		LightObjectColor(9).FadeSpeedDown = LightObjectColor(1).Intensity /  1
		LightObjectColor(10).FadeSpeedDown = LightObjectColor(1).Intensity /  1
		LightObjectColor(11).FadeSpeedDown = LightObjectColor(1).Intensity /  1
		LightObjectColor(12).FadeSpeedDown = LightObjectColor(1).Intensity /  1
		Lightcounter2 = Lightcounter2 + 1
	End If	
	If Lightcounter2 = 36 Then
		Lightcounter2 = 1
		LampRoundAnimation5.Enabled=False
		TurnOffRoundLights
		animation_start_and_finish_flag = 1
	End If
End Sub

Sub LampRoundAnimation6_Timer()	
	If lightstateflag = 1 Then	
		reverseLightcounter = 13 - Lightcounter
		
		LightObjectColor(reverseLightcounter).FadeSpeedDown = LightObjectColor(1).Intensity /  1
		LightObjectColor(reverseLightcounter).state = 1
		lightstateflag = 0
		
		'LampRoundAnimation6.interval= 200
	Else	
		reverseLightcounter2 = 13 - Lightcounter
		Lightcounter = Lightcounter + 1
		'LightObjectColor(reverseLightcounter2).Intensity= 500
		LightObjectColor(reverseLightcounter2).state = 0
		lightstateflag = 1
		'LampRoundAnimation6.interval= 200
	End If
	If Lightcounter = 13 Then
		Lightcounter = 1
		LampRoundAnimation6.Enabled=False
		animation_start_and_finish_flag = 1
	End If
End Sub
'LightObjectColor(12).FadeSpeedDown = LightObjectColor(1).Intensity /  10

Sub LampRoundAnimation7_Timer()	
	If Lightcounter2 = 3 Then
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 2 Then
		'lightstateflag = 1
'		TurnOffRoundLights
		LampRoundAnimation7.interval= 20
		LightObjectColor(1).state = 0
		LightObjectColor(2).state = 0
		LightObjectColor(3).state = 0
		LightObjectColor(4).state = 0
		LightObjectColor(5).state = 0
		LightObjectColor(6).state = 0
		LightObjectColor(7).state = 0
		LightObjectColor(8).state = 0
		LightObjectColor(9).state = 0
		LightObjectColor(10).state = 0
		LightObjectColor(11).state = 0
		LightObjectColor(12).state = 0
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 1 Then
		TurnOffRoundLights
		'lightstateflag = 1
	End If
	If Lightcounter2 = 1 Then	
		'lightstateflag = 0
		LampRoundAnimation7.interval= 800
		LightObjectColor(1).FadeSpeedDown = LightObjectColor(1).Intensity /  800000000
		LightObjectColor(1).state = 1
		LightObjectColor(2).FadeSpeedDown = LightObjectColor(1).Intensity /  800000000
		LightObjectColor(2).state = 1
		LightObjectColor(3).FadeSpeedDown = LightObjectColor(1).Intensity /  800000000
		LightObjectColor(3).state = 1
		LightObjectColor(4).FadeSpeedDown = LightObjectColor(1).Intensity /  800000000
		LightObjectColor(4).state = 1
		LightObjectColor(5).FadeSpeedDown = LightObjectColor(1).Intensity /  800000000
		LightObjectColor(5).state = 1
		LightObjectColor(6).FadeSpeedDown = LightObjectColor(1).Intensity /  800000000
		LightObjectColor(6).state = 1
		LightObjectColor(7).FadeSpeedDown = LightObjectColor(1).Intensity /  800000000
		LightObjectColor(7).state = 1
		LightObjectColor(8).FadeSpeedDown = LightObjectColor(1).Intensity /  800000000
		LightObjectColor(8).state = 1
		LightObjectColor(9).FadeSpeedDown = LightObjectColor(1).Intensity /  800000000
		LightObjectColor(9).state = 1
		LightObjectColor(10).FadeSpeedDown = LightObjectColor(1).Intensity /  800000000
		LightObjectColor(10).state = 1
		LightObjectColor(11).FadeSpeedDown = LightObjectColor(1).Intensity /  800000000
		LightObjectColor(11).state = 1
		LightObjectColor(12).FadeSpeedDown = LightObjectColor(1).Intensity /  800000000
		LightObjectColor(12).state = 1
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 4 Then
		Lightcounter2 = 1
		LampRoundAnimation7.Enabled=False
		'ColorRoundLight = 1
		TurnOffRoundLights
		animation_start_and_finish_flag = 1
	End If

End Sub

Sub LampRoundAnimation8_Timer()	
	If Lightcounter2 = 1 Then
		TurnOffRoundLights
	End If
	If lightstateflag = 1 Then	
		lightstateflag = 0
	Else	
		lightstateflag = 1
'		TurnOffRoundLights
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 2 Then
		Lightcounter2 = 1
		LampRoundAnimation8.Enabled=False
		'ColorRoundLight = 1
		TurnOffRoundLights
		animation_start_and_finish_flag = 1
	End If
End Sub

Sub LampRoundAnimation9_Timer()	
	If Lightcounter2 = 4 Then
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 3 Then
		lightstateflag = 1
		LightObjectColor(1).color = RGB(0, 0, 255)								'Color change only for Light L26
		LightObjectColor(1).colorfull = RGB(0, 0, 255)							'Color change only for Light L26
		LightObjectColor(1).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(1).state = 0
		LightObjectColor(1).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(1).state = 1
		LightObjectColor(2).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(2).state = 0
		LightObjectColor(3).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(3).state = 1
		LightObjectColor(4).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(4).state = 1
		LightObjectColor(5).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(5).state = 1
		LightObjectColor(6).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(6).state = 0
		LightObjectColor(7).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(7).state = 1
		LightObjectColor(8).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(8).state = 1
		LightObjectColor(9).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(9).state = 1
		LightObjectColor(10).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(10).state = 0
		LightObjectColor(11).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(11).state = 0
		LightObjectColor(12).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(12).state = 0
		LightObjectColor(11).color = RGB(0, 0, 255)								'Color change only for Light L56
		LightObjectColor(11).colorfull = RGB(0, 0, 255)							'Color change only for Light L56
		LightObjectColor(12).color = RGB(0, 0, 255)								'Color change only for Light L59
		LightObjectColor(12).colorfull = RGB(0, 0, 255)							'Color change only for Light L59
		LightObjectColor(11).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(11).state = 1
		LightObjectColor(12).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(12).state = 1
		Lightcounter2 = Lightcounter2 + 1
		'LampRoundAnimation2.interval= 200
	End If
	If Lightcounter2 = 2 Then
		lightstateflag = 1
		LightObjectColor(1).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(1).state = 1
		LightObjectColor(2).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(2).state = 0
		LightObjectColor(3).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(3).state = 1
		LightObjectColor(4).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(4).state = 1
		LightObjectColor(5).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(5).state = 1
		LightObjectColor(6).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(6).state = 0
		LightObjectColor(7).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(7).state = 1
		LightObjectColor(8).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(8).state = 1
		LightObjectColor(9).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(9).state = 1
		LightObjectColor(10).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(10).state = 0
		LightObjectColor(11).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(11).state = 1
		LightObjectColor(12).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(12).state = 1
		'LampRoundAnimation2.interval= 200
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 1 Then
		TurnOffRoundLights
	End If
	If Lightcounter2 = 1 Then	
		lightstateflag = 0
		LightObjectColor(1).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(1).state = 0
		LightObjectColor(2).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(2).state = 1
		LightObjectColor(3).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(3).state = 0
		LightObjectColor(4).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(4).state = 1
		LightObjectColor(5).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(5).state = 0
		LightObjectColor(6).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(6).state = 1
		LightObjectColor(7).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(7).state = 0
		LightObjectColor(8).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(8).state = 1
		LightObjectColor(9).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(9).state = 0
		LightObjectColor(10).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(10).state = 1
		LightObjectColor(11).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(11).state = 0
		LightObjectColor(12).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(12).state = 1
		Lightcounter2 = Lightcounter2 + 1
		'LampRoundAnimation2.interval= 200
	End If
	If Lightcounter2 = 5 Then
		Lightcounter2 = 1
		LampRoundAnimation9.Enabled=False
		'ColorRoundLight = 1
		TurnOffRoundLights
		animation_start_and_finish_flag = 1
	End If
End Sub
Sub LampRoundAnimation10_Timer()	
	If Lightcounter2 = 1 Then
		TurnOffRoundLights
		'lightstateflag = 0
	End If
	If Lightcounter2 = 5 Then
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 4 Then
		LightObjectColor(3).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(3).state = 1
		LightObjectColor(9).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(9).state = 1
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 3 Then
		LightObjectColor(4).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(4).state = 1
		LightObjectColor(8).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(8).state = 1
		LightObjectColor(2).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(2).state = 1
		LightObjectColor(10).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(10).state = 1
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 2 Then
		LightObjectColor(5).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(5).state = 1
		LightObjectColor(7).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(7).state = 1
		LightObjectColor(1).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(1).state = 1
		LightObjectColor(11).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(11).state = 1
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 1 Then	
		LightObjectColor(6).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(6).state = 1
		LightObjectColor(12).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(12).state = 1
		Lightcounter2 = Lightcounter2 + 1
	End If
	If LightCounter2 = 6 Then
		Lightcounter2 = 1
		LampRoundAnimation10.Enabled=False
		TurnOffRoundLights
		animation_start_and_finish_flag = 1
	End If	
End Sub
Sub LampRoundAnimation11_Timer()	
	If Lightcounter2 = 1 Then
		TurnOffRoundLights
		'lightstateflag = 0
	End If
	If Lightcounter2 = 8 Then
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 7 Then
		LightObjectColor(5).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(5).state = 1
		LightObjectColor(6).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(6).state = 1
		LightObjectColor(7).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(7).state = 1
		LightObjectColor(1).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(1).state = 1
		LightObjectColor(11).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(11).state = 1
		LightObjectColor(12).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(12).state = 1
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 6 Then
		LightObjectColor(5).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(5).state = 0
		LightObjectColor(6).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(6).state = 0
		LightObjectColor(7).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(7).state = 0
		LightObjectColor(1).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(1).state = 0
		LightObjectColor(11).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(11).state = 0
		LightObjectColor(12).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(12).state = 0
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 5 Then
		LightObjectColor(5).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(5).state = 1
		LightObjectColor(6).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(6).state = 1
		LightObjectColor(7).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(7).state = 1
		LightObjectColor(1).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(1).state = 1
		LightObjectColor(11).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(11).state = 1
		LightObjectColor(12).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(12).state = 1
		Lightcounter2 = Lightcounter2 + 1
	End If

	If Lightcounter2 = 4 Then
		LightObjectColor(6).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(6).state = 0
		LightObjectColor(12).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(12).state = 0
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 3 Then
		LightObjectColor(6).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(6).state = 1
		LightObjectColor(12).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(12).state = 1
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 2 Then
		LightObjectColor(6).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(6).state = 0
		LightObjectColor(12).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(12).state = 0
		Lightcounter2 = Lightcounter2 + 1
	End If
	If Lightcounter2 = 1 Then
		LightObjectColor(6).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(6).state = 1
		LightObjectColor(12).FadeSpeedDown = LightObjectColor(1).Intensity /  10
		LightObjectColor(12).state = 1
		Lightcounter2 = Lightcounter2 + 1
	End If

	If LightCounter2 = 9 Then
		Lightcounter2 = 1
		LampRoundAnimation11.Enabled=False
		TurnOffRoundLights
		animation_start_and_finish_flag = 1
	End If	
End Sub

Sub LampRoundAnimation12_Timer()	
	If Lightcounter2 = 1 Then
		LightObjectColor(1).BlinkPattern = "10"  
		LightObjectColor(1).BlinkInterval=50
		LightObjectColor(1).state = 2
		LightObjectColor(2).BlinkPattern = "01"
		LightObjectColor(2).BlinkInterval=50
		LightObjectColor(2).state = 2
		LightObjectColor(3).BlinkPattern = "10"   
		LightObjectColor(3).BlinkInterval=50
		LightObjectColor(3).state = 2
		LightObjectColor(4).BlinkPattern = "01" 
		LightObjectColor(4).BlinkInterval=50
		LightObjectColor(4).state = 2
		LightObjectColor(5).BlinkPattern = "10"   
		LightObjectColor(5).BlinkInterval=50
		LightObjectColor(5).state = 2
		LightObjectColor(6).BlinkPattern = "01" 
		LightObjectColor(6).BlinkInterval=50
		LightObjectColor(6).state = 2
		LightObjectColor(7).BlinkPattern = "10"   
		LightObjectColor(7).BlinkInterval=50
		LightObjectColor(7).state = 2
		LightObjectColor(8).BlinkPattern = "01" 
		LightObjectColor(8).BlinkInterval=50
		LightObjectColor(8).state = 2
		LightObjectColor(9).BlinkPattern = "10"   
		LightObjectColor(9).BlinkInterval=50
		LightObjectColor(9).state = 2
		LightObjectColor(10).BlinkPattern = "01" 
		LightObjectColor(10).BlinkInterval=50
		LightObjectColor(10).state = 2
		LightObjectColor(11).BlinkPattern = "10"   
		LightObjectColor(11).BlinkInterval=50
		LightObjectColor(11).state = 2
		LightObjectColor(12).BlinkPattern = "01" 
		LightObjectColor(12).BlinkInterval=50
		LightObjectColor(12).state = 2
	End If	
	If Lightcounter2 = 12 Then
		Lightcounter2 = 1
		LampRoundAnimation12.Enabled=False
		'ColorRoundLight = 1
		TurnOffRoundLights
		lightstateflag = 1
		animation_start_and_finish_flag = 1
	End If
Lightcounter2 = Lightcounter2 + 1
End Sub
'********************************************
'*****GI Lights On***************************
'********************************************

'dim xx
'For each xx in GI:xx.State = 1: Next


'********************************************
'**********KEYS DOWN*************************
'********************************************


Sub TurnOffRoundLights()
	Dim a
		For each a in RoundLights
			a.FadeSpeedDown = a.Intensity /  10
			a.State = 0
		Next
End Sub

Sub SaveStateLightturnlight()							'new sub for problem : when the ball is drained if you push on the right or leftflipper a new state 1 is added
	OldState_L12(CurrentPlayer) = L12.state
	OldState_L15(CurrentPlayer) = L15.state
	OldState_L16(CurrentPlayer) = L16.state
	OldState_L11(CurrentPlayer) = L11.state
	OldState_L116(CurrentPlayer) = L116.state
	OldState_L117(CurrentPlayer) = L117.state
	OldState_L115(CurrentPlayer) = L115.state
'	OldState_L131(CurrentPlayer) = L131.state
'	OldState_L132(CurrentPlayer) = L132.state
'	OldState_L133(CurrentPlayer) = L133.state
End Sub

Sub SaveStateLight()
	OldState_L101(CurrentPlayer)=L101.state
	OldState_L103(CurrentPlayer)=L103.state
	OldState_L106(CurrentPlayer)=L106.state
'	OldState_L11(CurrentPlayer)=L11.state
	OldState_L110(CurrentPlayer)=L110.state
	OldState_L111(CurrentPlayer)=L111.state
	OldState_L112(CurrentPlayer)=L112.state
'	OldState_L115(CurrentPlayer)=L115.state
'	OldState_L116(CurrentPlayer)=L116.state
'	OldState_L117(CurrentPlayer)=L117.state
'	OldState_L12(CurrentPlayer)=L12.state
	OldState_L13(CurrentPlayer)=L13.state
'	OldState_L131(CurrentPlayer)=L131.state
'	OldState_L132(CurrentPlayer)=L132.state
'	OldState_L133(CurrentPlayer)=L133.state
	OldState_L14(CurrentPlayer)=L14.state
'	OldState_L15(CurrentPlayer)=L15.state
'	OldState_L16(CurrentPlayer)=L16.state
	OldState_L163(CurrentPlayer)=L163.state
	OldState_L165(CurrentPlayer)=L165.state
	OldState_L166(CurrentPlayer)=L166.state
	OldState_L170(CurrentPlayer)=L170.state
	OldState_L171(CurrentPlayer)=L171.state
	OldState_L172(CurrentPlayer)=L172.state
	OldState_L18(CurrentPlayer)=L18.state
	OldState_L19(CurrentPlayer)=L19.state
	OldState_L20(CurrentPlayer)=L20.state
	OldState_L21(CurrentPlayer)=L21.state
	OldState_L22(CurrentPlayer)=L22.state
	OldState_L23(CurrentPlayer)=L23.state
	OldState_L24(CurrentPlayer)=L24.state
'	OldState_L26(CurrentPlayer)=L26.state
'	OldState_L29(CurrentPlayer)=L29.state
'	OldState_L32(CurrentPlayer)=L32.state
'	OldState_L35(CurrentPlayer)=L35.state
'	OldState_L38(CurrentPlayer)=L38.state
'	OldState_L41(CurrentPlayer)=L41.state
'	OldState_L44(CurrentPlayer)=L44.state
'	OldState_L47(CurrentPlayer)=L47.state
'	OldState_L50(CurrentPlayer)=L50.state
'	OldState_L53(CurrentPlayer)=L53.state
'	OldState_L56(CurrentPlayer)=L56.state
'	OldState_L59(CurrentPlayer)=L59.state
	OldState_L63(CurrentPlayer)=L63.state
	OldState_L64(CurrentPlayer)=L64.state
	OldState_L66(CurrentPlayer)=L66.state
	OldState_L69(CurrentPlayer)=L69.state
	OldState_L70(CurrentPlayer)=L70.state
	OldState_L72(CurrentPlayer)=L72.state
	OldState_L75(CurrentPlayer)=L75.state
	OldState_L79(CurrentPlayer)=L79.state
	OldState_L82(CurrentPlayer)=L82.state
	OldState_L83(CurrentPlayer)=L83.state
	OldState_L85(CurrentPlayer)=L85.state
	OldState_L88(CurrentPlayer)=L88.state
	OldState_L89(CurrentPlayer)=L89.state
	OldState_L91(CurrentPlayer)=L91.state
	OldState_L94(CurrentPlayer)=L94.state
	OldState_L95(CurrentPlayer)=L95.state
	OldState_L96(CurrentPlayer)=L96.state
	OldState_L98(CurrentPlayer)=L98.state
	OldState_Light026(CurrentPlayer)=Light026.state
	OldState_Light054(CurrentPlayer)=Light054.state
	OldState_Light055(CurrentPlayer)=Light055.state
	OldState_Light056(CurrentPlayer)=Light056.state
	OldState_Light057(CurrentPlayer)=Light057.state
	OldState_Light058(CurrentPlayer)=Light058.state
	OldState_Light059(CurrentPlayer)=Light059.state
	OldState_Light060(CurrentPlayer)=Light060.state
	OldState_Light061(CurrentPlayer)=Light061.state
	OldState_Light062(CurrentPlayer)=Light062.state
	OldState_Light063(CurrentPlayer)=Light063.state
	'OldState_L13 = L13.state
	'OldAllLights = AllLights
End Sub

Sub TurnOffAllLights()
	RampChangeStatus(CurrentPlayer) = "OFF"
	Dim a
		For each a in AllLights
			a.State = 0
		Next
End Sub

Sub TurnOffAllRegularLights()
	RampChangeStatus(CurrentPlayer) = "OFF"
			  
			  
			  
			  
			  
			  
			  
			  
			  
			  
			  
			   
	Dim a
		For each a in AnimationRegular2
			a.State = 0
		Next
End Sub

'************************************************************************************
'
'
'						Light Sequence Animation Timer
'						------------------------------
'
'
'
'seq_animation_array(Number of color choice + animation choice, Color choice 1, Animation choice 1, Color choice 2, Animation choice 2, ETC... )
'seq_animation_array(X,0-6,7-17,0-6,7-17,etc...)					
'
'EG:
'seq_animation_array(4,0,10,1,12)
'LightsequenceAnimation.Enabled=True
'************************************************************************************

Sub LightsequenceAnimation_timer()	'
'	CheckLamp.Enabled = False
	CheckLamp.Enabled = False
	MissionRandomTimer.Enabled=False
	If animation_start_and_finish_flag = 1 Then
		If SeqAnimation <> seq_animation_array(0) Then
			SeqAnimation = SeqAnimation + 1
			Select Case seq_animation_array(SeqAnimation)
			'	Select Case ColorRoundLight				'ColorRoundLight : 0 = Red | 1 = Green | 2 = Blue | 3 = Yellow | 4 = white | 5 = Magenta | 6 = Orange
				Case 0 : LightObjectColor(1).color = RGB(255, 0, 0) : LightObjectColor(1).colorfull = RGB(255, 0, 0) : LightObjectColor(2).color = RGB(255, 0, 0) : LightObjectColor(2).colorfull = RGB(255, 0, 0) : LightObjectColor(3).color = RGB(255, 0, 0) : LightObjectColor(3).colorfull = RGB(255, 0, 0) : LightObjectColor(4).color = RGB(255, 0, 0) : LightObjectColor(4).colorfull = RGB(255, 0, 0) : LightObjectColor(5).color = RGB(255, 0, 0) : LightObjectColor(5).colorfull = RGB(255, 0, 0) : LightObjectColor(6).color = RGB(255, 0, 0) : LightObjectColor(6).colorfull = RGB(255, 0, 0) : LightObjectColor(7).color = RGB(255, 0, 0) : LightObjectColor(7).colorfull = RGB(255, 0, 0) : LightObjectColor(8).color = RGB(255, 0, 0) : LightObjectColor(8).colorfull = RGB(255, 0, 0) : LightObjectColor(9).color = RGB(255, 0, 0) : LightObjectColor(9).colorfull = RGB(255, 0, 0) : LightObjectColor(10).color = RGB(255, 0, 0) : LightObjectColor(10).colorfull = RGB(255, 0, 0) : LightObjectColor(11).color = RGB(255, 0, 0) : LightObjectColor(11).colorfull = RGB(255, 0, 0) : LightObjectColor(12).color = RGB(255, 0, 0) : LightObjectColor(12).colorfull = RGB(255, 0, 0) : 'RED
				Case 1 : LightObjectColor(1).color = RGB(0, 255, 0) : LightObjectColor(1).colorfull = RGB(0, 255, 0) : LightObjectColor(2).color = RGB(0, 255, 0) : LightObjectColor(2).colorfull = RGB(0, 255, 0) : LightObjectColor(3).color = RGB(0, 255, 0) : LightObjectColor(3).colorfull = RGB(0, 255, 0) : LightObjectColor(4).color = RGB(0, 255, 0) : LightObjectColor(4).colorfull = RGB(0, 255, 0) : LightObjectColor(5).color = RGB(0, 255, 0) : LightObjectColor(5).colorfull = RGB(0, 255, 0) : LightObjectColor(6).color = RGB(0, 255, 0) : LightObjectColor(6).colorfull = RGB(0, 255, 0) : LightObjectColor(7).color = RGB(0, 255, 0) : LightObjectColor(7).colorfull = RGB(0, 255, 0) : LightObjectColor(8).color = RGB(0, 255, 0) : LightObjectColor(8).colorfull = RGB(0, 255, 0) : LightObjectColor(9).color = RGB(0, 255, 0) : LightObjectColor(9).colorfull = RGB(0, 255, 0) : LightObjectColor(10).color = RGB(0, 255, 0) : LightObjectColor(10).colorfull = RGB(0, 255, 0) : LightObjectColor(11).color = RGB(0, 255, 0) : LightObjectColor(11).colorfull = RGB(0, 255, 0) : LightObjectColor(12).color = RGB(0, 255, 0) : LightObjectColor(12).colorfull = RGB(0, 255, 0) : 'GREEN
				Case 2 : LightObjectColor(1).color = RGB(0, 0, 255) : LightObjectColor(1).colorfull = RGB(0, 0, 255) : LightObjectColor(2).color = RGB(0, 0, 255) : LightObjectColor(2).colorfull = RGB(0, 0, 255) : LightObjectColor(3).color = RGB(0, 0, 255) : LightObjectColor(3).colorfull = RGB(0, 0, 255) : LightObjectColor(4).color = RGB(0, 0, 255) : LightObjectColor(4).colorfull = RGB(0, 0, 255) : LightObjectColor(5).color = RGB(0, 0, 255) : LightObjectColor(5).colorfull = RGB(0, 0, 255) : LightObjectColor(6).color = RGB(0, 0, 255) : LightObjectColor(6).colorfull = RGB(0, 0, 255) : LightObjectColor(7).color = RGB(0, 0, 255) : LightObjectColor(7).colorfull = RGB(0, 0, 255) : LightObjectColor(8).color = RGB(0, 0, 255) : LightObjectColor(8).colorfull = RGB(0, 0, 255) : LightObjectColor(9).color = RGB(0, 0, 255) : LightObjectColor(9).colorfull = RGB(0, 0, 255) : LightObjectColor(10).color = RGB(0, 0, 255) : LightObjectColor(10).colorfull = RGB(0, 0, 255) : LightObjectColor(11).color = RGB(0, 0, 255) : LightObjectColor(11).colorfull = RGB(0, 0, 255) : LightObjectColor(12).color = RGB(0, 0, 255) : LightObjectColor(12).colorfull = RGB(0, 0, 255) : 'BLUE
				Case 3 : LightObjectColor(1).color = RGB(255, 255, 0) : LightObjectColor(1).colorfull = RGB(255, 255, 0) : LightObjectColor(2).color = RGB(255, 255, 0) : LightObjectColor(2).colorfull = RGB(255, 255, 0) : LightObjectColor(3).color = RGB(255, 255, 0) : LightObjectColor(3).colorfull = RGB(255, 255, 0) : LightObjectColor(4).color = RGB(255, 255, 0) : LightObjectColor(4).colorfull = RGB(255, 255, 0) : LightObjectColor(5).color = RGB(255, 255, 0) : LightObjectColor(5).colorfull = RGB(255, 255, 0) : LightObjectColor(6).color = RGB(255, 255, 0) : LightObjectColor(6).colorfull = RGB(255, 255, 0) : LightObjectColor(7).color = RGB(255, 255, 0) : LightObjectColor(7).colorfull = RGB(255, 255, 0) : LightObjectColor(8).color = RGB(255, 255, 0) : LightObjectColor(8).colorfull = RGB(255, 255, 0) : LightObjectColor(9).color = RGB(255, 255, 0) : LightObjectColor(9).colorfull = RGB(255, 255, 0) : LightObjectColor(10).color = RGB(255, 255, 0) : LightObjectColor(10).colorfull = RGB(255, 255, 0) : LightObjectColor(11).color = RGB(255, 255, 0) : LightObjectColor(11).colorfull = RGB(255, 255, 0) : LightObjectColor(12).color = RGB(255, 255, 0) : LightObjectColor(12).colorfull = RGB(255, 255, 0) : 'YELLOW
				Case 4 : LightObjectColor(1).color = RGB(255, 255, 255) : LightObjectColor(1).colorfull = RGB(255, 255, 255) : LightObjectColor(2).color = RGB(255, 255, 255) : LightObjectColor(2).colorfull = RGB(255, 255, 255) : LightObjectColor(3).color = RGB(255, 255, 255) : LightObjectColor(3).colorfull = RGB(255, 255, 255) : LightObjectColor(4).color = RGB(255, 255, 255) : LightObjectColor(4).colorfull = RGB(255, 255, 255) : LightObjectColor(5).color = RGB(255, 255, 255) : LightObjectColor(5).colorfull = RGB(255, 255, 255) : LightObjectColor(6).color = RGB(255, 255, 255) : LightObjectColor(6).colorfull = RGB(255, 255, 255) : LightObjectColor(7).color = RGB(255, 255, 255) : LightObjectColor(7).colorfull = RGB(255, 255, 255) : LightObjectColor(8).color = RGB(255, 255, 255) : LightObjectColor(8).colorfull = RGB(255, 255, 255) : LightObjectColor(9).color = RGB(255, 255, 255) : LightObjectColor(9).colorfull = RGB(255, 255, 255) : LightObjectColor(10).color = RGB(255, 255, 255) : LightObjectColor(10).colorfull = RGB(255, 255, 255) : LightObjectColor(11).color = RGB(255, 255, 255) : LightObjectColor(11).colorfull = RGB(255, 255, 255) : LightObjectColor(12).color = RGB(255, 255, 255) : LightObjectColor(12).colorfull = RGB(255, 255, 255) : 'WHITE
				Case 5 : LightObjectColor(1).color = RGB(255, 0, 255) : LightObjectColor(1).colorfull = RGB(255, 0, 255) : LightObjectColor(2).color = RGB(255, 0, 255) : LightObjectColor(2).colorfull = RGB(255, 0, 255) : LightObjectColor(3).color = RGB(255, 0, 255) : LightObjectColor(3).colorfull = RGB(255, 0, 255) : LightObjectColor(4).color = RGB(255, 0, 255) : LightObjectColor(4).colorfull = RGB(255, 0, 255) : LightObjectColor(5).color = RGB(255, 0, 255) : LightObjectColor(5).colorfull = RGB(255, 0, 255) : LightObjectColor(6).color = RGB(255, 0, 255) : LightObjectColor(6).colorfull = RGB(255, 0, 255) : LightObjectColor(7).color = RGB(255, 0, 255) : LightObjectColor(7).colorfull = RGB(255, 0, 255) : LightObjectColor(8).color = RGB(255, 0, 255) : LightObjectColor(8).colorfull = RGB(255, 0, 255) : LightObjectColor(9).color = RGB(255, 0, 255) : LightObjectColor(9).colorfull = RGB(255, 0, 255) : LightObjectColor(10).color = RGB(255, 0, 255) : LightObjectColor(10).colorfull = RGB(255, 0, 255) : LightObjectColor(11).color = RGB(255, 0, 255) : LightObjectColor(11).colorfull = RGB(255, 0, 255) : LightObjectColor(12).color = RGB(255, 0, 255) : LightObjectColor(12).colorfull = RGB(255, 0, 255) : 'MAGENTA
				Case 6 : LightObjectColor(1).color = RGB(255, 50, 0) : LightObjectColor(1).colorfull = RGB(255, 50, 0) : LightObjectColor(2).color = RGB(255, 50, 0) : LightObjectColor(2).colorfull = RGB(255, 50, 0) : LightObjectColor(3).color = RGB(255, 50, 0) : LightObjectColor(3).colorfull = RGB(255, 50, 0) : LightObjectColor(4).color = RGB(255, 50, 0) : LightObjectColor(4).colorfull = RGB(255, 50, 0) : LightObjectColor(5).color = RGB(255, 50, 0) : LightObjectColor(5).colorfull = RGB(255, 50, 0) : LightObjectColor(6).color = RGB(255, 50, 0) : LightObjectColor(6).colorfull = RGB(255, 50, 0) : LightObjectColor(7).color = RGB(255, 50, 0) : LightObjectColor(7).colorfull = RGB(255, 50, 0) : LightObjectColor(8).color = RGB(255, 50, 0) : LightObjectColor(8).colorfull = RGB(255, 50, 0) : LightObjectColor(9).color = RGB(255, 50, 0) : LightObjectColor(9).colorfull = RGB(255, 50, 0) : LightObjectColor(10).color = RGB(255, 50, 0) : LightObjectColor(10).colorfull = RGB(255, 50, 0) : LightObjectColor(11).color = RGB(255, 50, 0) : LightObjectColor(11).colorfull = RGB(255, 50, 0) : LightObjectColor(12).color = RGB(255, 50, 0) : LightObjectColor(12).colorfull = RGB(255, 50, 0) : 'ORANGE
				case 7 :
					LampRoundAnimation.Enabled=True						'ANIMATION 1		7 (light on turn clockwise)
					animation_start_and_finish_flag = 0
				case 8 :
					LampRoundAnimation2.Enabled=True					'ANIMATION 2		8 (Even and odd lamps light up alternately.)
					animation_start_and_finish_flag = 0
				case 9 :
					LampRoundAnimation3.Enabled=True					'ANIMATION 3		9 (All lamps in the circle flash 4 times)
					animation_start_and_finish_flag = 0
				case 10:
					LampRoundAnimation4.Enabled=True					'ANIMATION 4		10(Circle lamps light up from bottom to top - used for Hydra)
					animation_start_and_finish_flag = 0
				case 11 :
					LampRoundAnimation5.Enabled=True					'ANIMATION 5		11 (The lamps rotate counterclockwise and the number of lit lamps increases to completely fill the circle)
					animation_start_and_finish_flag = 0
				case 12 :
					LampRoundAnimation6.Enabled=True					'ANIMATION 6		12 (light on turn counterclockwise)
					animation_start_and_finish_flag = 0
				case 13 :
					LampRoundAnimation7.Enabled=True					'ANIMATION 7		13 (round lights turn on for 800ms and shutdown in 20ms) 
					animation_start_and_finish_flag = 0
				case 14 :
					LampRoundAnimation8.Enabled=True					'ANIMATION 8		14 (all round lights is off)
					animation_start_and_finish_flag = 0
				case 15 :
					LampRoundAnimation9.Enabled=True					'ANIMATION 9		15 (Even lamps light up, | 11,12,1 + 3,4,5 + 7,8,9 is up with the choice color|  11,12,1->(in blue) + (3,4,5 + 7,8,9) Choice color) used for Deep Freeze
					animation_start_and_finish_flag = 0
				case 16 :
					LampRoundAnimation10.Enabled=True					'ANIMATION 10		16 (the top and bottom lamps light up gradually to close the circle) used for Black Castle
					animation_start_and_finish_flag = 0
				case 17 :
					LampRoundAnimation11.Enabled=True					'ANIMATION 11		17 (lamps 12 and 6 flash 2x then, lamps 11,12,1 and 5,6,7 flash 2x) used for Black Castle
					animation_start_and_finish_flag = 0
				case 18 :
					LampRoundAnimation12.Enabled=True					'ANIMATION 12		18 (50% of the lamp is On the rest is OFF, the next sequence is reverse)
					animation_start_and_finish_flag = 0
			End Select	
		End If 
	End If
	If SeqAnimation = seq_animation_array(0) Then
		If animation_start_and_finish_flag = 1 Then
			SeqAnimation = 0
			LightsequenceAnimation.Enabled = False
			EndLightsequenceAnimation.Enabled=True
		End If
	End If
End Sub

Sub EndLightsequenceAnimation_timer()
	EndLightsequenceAnimation.Enabled=False
	relighttable
	CheckLamp.interval=150
	CheckLamp.Enabled=True
	'MissionRandomTimer.Enabled=True
	'TurnOnAmbiantLight
	
End Sub



'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'   TABLE INITS & MATHS
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  

Sub StartingPinball_timer()
'	Playsound "Sound-0x000C"
	PlaySound "Sound-0x000C", 0, SongVolume
	StartingPinball.Enabled=False
	PleaseWait = False
'	video_init
End Sub

Sub Table1_Init()
'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
'		B2SOn = 1 
'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		BallsRemaining(0)=bpgcurrent
		SpeakTime = 1000
'		ResetValue			'Add HighScore Stern (replaced by another Sub)
'		ResetHs				'Clear Everything
		BallControlTimer.Enabled = False
		RetroMode = 0
		mMagnetSave.MagnetOn = 0
		LastChanceStart = False
		'compteuri = 901
		LightChecked3x = 0
		RetroMode = 0
		HideOverlay = False
		PleaseWait = True
		CheckLampAction.Enabled=False
		if notinus = 1 then
			setlocale(1033)
		end If
		Lightcounter = 1
		Lightcounter2 = 1
		StartMission = 0
'		video_init
		Musicball = 1
		musicpriority = 1
		resetbackglass

'SaveValue TableName, "BKSOR01", "XXX"
'SaveValue TableName, "BKSOR02", 1

		LoadEM
		Dim i
		Randomize
		Loadhs
		bAttractMode = False
		bOnTheFirstBall = False
		bBallInPlungerLane = False
		bBallSaverActive = False
		bBallSaverReady = False
		bMultiBallMode = False
		bGameInPlay = False
		bAutoPlunger = False
		bMusicOn = True
		BallsOnPlayfield = 0
		BallInLocker = 0
		BallsInHole = 0
		LastSwitchHit = ""
		Tilt = 0
		TiltSensitivity = 6
		Tilted = False
		bBonusHeld = False
		bJustStarted = True
		GiOff
		'PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":4, ""FS"":1 }"
		StartAttractMode
	    if usePUPDMD Then 
			PUPInit
			Else 
			pupDMDupdate.enabled=0  'init pupdmd driver
        End if
	End Sub


	'********************
	' MATHS
	'********************

	Function RndNum(min,max)
	 RndNum = Int(Rnd()*(max-min+1))+min     ' Sets a random number between min AND max
	End Function

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'   TABLE 1 KEYS
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  

Sub Table1_KeyDown(ByVal Keycode)
BallRemainingPrevision = BallsRemaining(CurrentPlayer) - 1
If PleaseWait = False Then
	If keycode = PlungerKey Then
		If EnableRetractPlunger Then
			Plunger.PullBackandRetract
		Else
			Plunger.PullBack
		End If
		PlaySound "plungerpull",0,1,AudioPan(Plunger),0.25,0,0,1,AudioFade(Plunger)
	End If

	IF keycode = PlungerKey And LastChanceStart = True Then									'MAGNA SAVE BUTTON IN THIS CASE IS "PlungerKey" --> Used For Last Chance
		AutoFireTimer.Enabled=true
	End If

	If keycode = StartGameKey And Credits>0 And BallsRemaining(CurrentPlayer)=3 And InProgress= False Then
		If PlayersPlayingGame <MaxPlayers Then
			PlayersPlayingGame = PlayersPlayingGame + 1
			Credits=Credits-1
			DOF 300, DOFOff
			If Credits = 0 Then
				DOF 200, DOFOff
			End IF
'			SaveCredits
'			Savehs
			Select Case Int(Rnd*3)+1
				Case 1 : audioknight = "Sound-0x01C3.mp3":SpeakTime = 2847	'Are you ready to fight for you live
				Case 2 : audioknight = "Sound-0x01E0.mp3":SpeakTime = 1630	'stand up and fight
				Case 3 : audioknight = "Sound-0x01FA.mp3"=SpeakTime = 3661 	'you? Hahahahahahahah
			End Select
			LightEyesBK
			playmedia audioknight,"Audioknight",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)	

'!!!		New TO BE CHECKED !!!!!!!!!!!!!!!!
			if canstart = 1 then exit sub
			If (bOnTheFirstBall = false) Then
				if retimct=1 Then
					endthisgame
				end if
				if retimct=0 Then
					wanttoend
				end if
			End if
'!!!	 	New TO BE CHECKED !!!!!!!!!!!!!!!!

			if bAttractMode = false Then
'				pupDMDDisplay "-","Please Wait^Game Resetting",dmdnote,3,0,10
				pNote "Please wait","game resetting"
			else
				ResetForNewGame()
'				InProgress=True 
			end if 		

			If PlayersPlayingGame = 1 Then
				VideoSetBackground
				restoreOverlay.Interval=100
				restoreOverlay.Enabled=True
				'playmedia "overlay.png","PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				playmedia "player1.mp3","audiocallouts",pCallouts,"",1000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'				pupDMDDisplay "-","Player^One",dmdnote,3,0,10
			End If
			If PlayersPlayingGame = 2 Then
				restoreOverlay.Interval=100
				restoreOverlay.Enabled=True
				'playmedia "overlay2p.png","PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				PuPlayer.LabelSet pBackglass,"Play2","PLAYER 2",1,"{'mt':2}"
				pUpdateScores
				playmedia "player2.mp3","audiocallouts",pCallouts,"",1000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'				pupDMDDisplay "-","Player^Two",dmdnote,3,0,10
			End If
			If PlayersPlayingGame = 3 Then
				restoreOverlay.Interval=100
				restoreOverlay.Enabled=True
'				playmedia "overlay3p.png","PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				PuPlayer.LabelSet pBackglass,"Play3","PLAYER 3",1,"{'mt':2}"
				pUpdateScores
				playmedia "player3.mp3","audiocallouts",pCallouts,"",1000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'				pupDMDDisplay "-","Player^Three",dmdnote,3,0,10
			End If
			If PlayersPlayingGame = 4 Then
				restoreOverlay.Interval=100
				restoreOverlay.Enabled=True
'				playmedia "overlay4p.png","PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				PuPlayer.LabelSet pBackglass,"Play4","PLAYER 4",1,"{'mt':2}"
				pUpdateScores	
				playmedia "player4.mp3","audiocallouts",pCallouts,"",1000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'				pupDMDDisplay "-","Player^Four",dmdnote,3,0,10
			End If
			TotalGamesPlayed = TotalGamesPlayed + 1
'			Savehs
'			savegp
			LightEyesBK
			playmedia audioknight,"Audioknight",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			'playmedia audioknight,"Audioknight",pAudio,"",0,"",1,4  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		End If
	End If

	IF keycode = AddCreditKey Then '5
		'compteuri = 901
		'Fireanimation.Enabled = True
		'	StopMainScreen		'In Test
		'	BumpVideo			'In Test

		Credits=Credits + 1
		DOF 200, DOFOn
		DOF 303, DOFPulse
'		Savehs
		SaveCredits
		If InProgress= False Then PlaySound "coin" 
			Select Case Int(Rnd*3)+1
				Case 1 : audioknight = "Sound-0x0196.mp3":SpeakTime = 1742
				Case 2 : audioknight =  "Sound-0x01A2.mp3":SpeakTime = 1536
				Case 3 : audioknight =  "Sound-0x01D1.mp3":SpeakTime = 2591
			End Select
			LightEyesBK
			playmedia audioknight,"Audioknight",pAudio,"",0,"",1,1  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		END If
	End If




	IF hsbModeActive = True Then
		EnterHighScoreKey(keycode)
	ElseIf bGameInPlay = False And LastChanceFlag = True And LastChanceStart = False And Gamerover_flag = False Then			'used when two balls are blocked on the upperfield
		If keycode = LeftTiltKey Then Nudge 90, 6:PlaySound SoundFX("fx_nudge",0), 0, 1, -0.1, 0.25								'used when two balls are blocked on the upperfield
		If keycode = RightTiltKey Then Nudge 270, 6:PlaySound SoundFX("fx_nudge",0), 0, 1, 0.1, 0.25							'used when two balls are blocked on the upperfield
		If keycode = CenterTiltKey Then Nudge 0, 7:PlaySound SoundFX("fx_nudge",0), 0, 1, 1, 0.25								'used when two balls are blocked on the upperfield
		If keycode = MechanicalTilt Then PlaySound SoundFX("fx_nudge",0), 0, 1, 1, 0.25
	ELSEIF bGameInPlay Then
		If keycode = LeftTiltKey Then Nudge 90, 6:PlaySound SoundFX("fx_nudge",0), 0, 1, -0.1, 0.25:CheckTilt
		If keycode = RightTiltKey Then Nudge 270, 6:PlaySound SoundFX("fx_nudge",0), 0, 1, 0.1, 0.25:CheckTilt
		If keycode = CenterTiltKey Then Nudge 0, 7:PlaySound SoundFX("fx_nudge",0), 0, 1, 1, 0.25:CheckTilt
		If keycode = MechanicalTilt Then Nudge 0, 7:PlaySound SoundFX("fx_nudge",0), 0, 1, 1, 0.25:CheckTilt
		If NOT Tilted and Not StopRansom Then
			If StopBallControlFlag = True And LastChanceStart = False and BallRemainingPrevision <= 0 And keycode = PlungerKey And ExtraBallsAwards(CurrentPlayer) = 0 Then
				LastChanceButtonPush(CurrentPlayer) = True
			End If
		If BallsOnPlayfield > 0 And StopBallControlFlag = False And LastChanceStart = False Then
			If MagnaSaveFlag(CurrentPlayer) = 1 Then
					
					 
							 
						  
				If keycode = PlungerKey Or keycode = LockbarKey Or keycode = 157 Then				'MAGNA SAVE BUTTON IN THIS CASE IS  "PlungerKey" or "LockbarKey" or "rtmagnasave" --> Used For Magna Saveg
					
					 
							 
						  
																																			   
				DOF 201, DOFOff
				Video_Magna_Save
				mMagnetSave.MagnetOn = 1
				magnettimer.enabled=1						  
				End If	
			End If
		End IF
		If keycode = LeftFlipperKey Then
			'*******************************************************************************
			If BypassVideo = True Then
				PuPlayer.playstop pBackglass
				EndOfVideo.Interval=510
				EndOfVideo.Enabled=True
				If video_catapult.enabled = True Then
					video_catapult.Interval =500
					video_catapult.enabled = True
		   
									   
							   
								
		   
									
						   
							 
											 
											
											
		   
										   
									
									
		   
											  
									   
									   
		   
												
									   
										 
		   
						
				End If
				If LockerTimer.Enabled = True Then
					LockerTimer.Interval =520
					LockerTimer.enabled = True
				End If
				If timer004.Enabled = True Then
					timer004.Interval=530
					timer004.Enabled = True
																				
								   
		
							
			  
					LightsequenceAnimation.Enabled = False 
					EndLightsequenceAnimation.Interval=540
					EndLightsequenceAnimation.Enabled=True
				End If
				If ShootToSkillTimer.enabled=True Then
					ShootToSkillTimer.Interval=500
					ShootToSkillTimer.enabled=True
				End If
				If CatapultManagement.Enabled = True Then
					CatapultManagement.interval = 510
					CatapultManagement.Enabled = True
				End If
				If KnightChallengeTimer.Enabled = True Then
					KnightChallengeTimer.Interval=570
					KnightChallengeTimer.Enabled = True
		  
											
											
											
											
											  
											  
											  
			
							 
				End If
				BypassVideo = False
			End If
			'*******************************************************************************
			If retimct=1 Then
				retimct=0
'				pupDMDDisplay "-","Quick Restart^Cancelled",dmdnote,3,0,30
			end if
			If PuPGameRunning Then
				PuPGameInfo= PuPlayer.GameUpdate("PupMiniGame", 1 , 83 , "")  'w
			Elseif LastChanceStart = True And CurrentMissionFlag(CurrentPlayer) = 3 Then
		
			'LeftFlipper doesn't rotate...
			Else
				LeftFlipper.RotateToEnd
				ldown = 1
				BallControlTimer.Enabled = False
				checkdown
'				movelanesleft
				PlaySound SoundFX("fx_Flipperup",DOFFlippers), 0, .67, AudioPan(LeftFlipper), 0.05,0,0,1,AudioFade(LeftFlipper)
				DOF 101, DOFOn
				If LightsequenceAnimation.Enabled = False Or BallsOnPlayfield > 0 Then
					SaveStateLightturnlight
					If BallsOnPlayfield > 0 Then
						L11.state=OldState_L12(CurrentPlayer)
						L12.state=OldState_L15(CurrentPlayer)
						L15.state=OldState_L16(CurrentPlayer)
						L16.state=OldState_L11(CurrentPlayer)
						L115.state=OldState_L116(CurrentPlayer)
						L116.state=OldState_L117(CurrentPlayer)
						L117.state=OldState_L115(CurrentPlayer)							
					Else
						L11.state=OldState_L11(CurrentPlayer)
						L12.state=OldState_L12(CurrentPlayer)
						L15.state=OldState_L15(CurrentPlayer)
						L16.state=OldState_L16(CurrentPlayer)
						L115.state=OldState_L115(CurrentPlayer)
						L116.state=OldState_L116(CurrentPlayer)
						L117.state=OldState_L117(CurrentPlayer)
					End If
					SaveStateLightturnlight
				End If

					 
			  
																
			end if
		End If
		If keycode = RightFlipperKey Then	
			If BypassVideo = True Then
				PuPlayer.playstop pBackglass
				EndOfVideo.Interval=510
				EndOfVideo.Enabled=True
				If video_catapult.enabled = True Then
					video_catapult.Interval =520
					video_catapult.enabled = True
				End If
					
																													  
				   
				If LockerTimer.Enabled = True Then
					LockerTimer.Interval =520
					LockerTimer.enabled = True
				End If
				If timer004.Enabled = True Then
					timer004.Interval=530
					timer004.Enabled = True
					LightsequenceAnimation.Enabled = False 
					EndLightsequenceAnimation.Interval=540
					EndLightsequenceAnimation.Enabled=True
				End If
				If ShootToSkillTimer.enabled=True Then
					ShootToSkillTimer.Interval=500
					ShootToSkillTimer.enabled=True
				End If
				If KnightChallengeTimer.Enabled = True Then
					KnightChallengeTimer.Interval=570
					KnightChallengeTimer.Enabled = True
				End If
					BypassVideo = False
		   
		  
			End If
   

			if retimct=1 Then
				retimct=0
'				pupDMDDisplay "-","Quick Restart^Cancelled",dmdnote,3,0,30
			end if
			if PuPGameRunning Then
				PuPGameInfo= PuPlayer.GameUpdate("PupMiniGame", 1 , 87 , "")  'w
			Elseif LastChanceStart = True And CurrentMissionFlag(CurrentPlayer) = 3 Then
					UPRightFlipper.RotateToEnd
					PlaySound SoundFX("fx_Flipperup",DOFFlippers), 0, .67, AudioPan(RightFlipper), 0.05,0,0,1,AudioFade(RightFlipper)
			Else
				RightFlipper.RotateToEnd:UPRightFlipper.RotateToEnd
				rdown = 1
				BallControlTimer.Enabled = False
				checkdown
'				movelanesright
				PlaySound SoundFX("fx_Flipperup",DOFFlippers), 0, .67, AudioPan(RightFlipper), 0.05,0,0,1,AudioFade(RightFlipper)
				DOF 102, DOFOn
				If LightsequenceAnimation.Enabled = False Or BallsOnPlayfield > 0 Then
					SaveStateLightturnlight
					If BallsOnPlayfield > 0 Then
						L11.state=OldState_L16(CurrentPlayer)
						L12.state=OldState_L11(CurrentPlayer)
						L15.state=OldState_L12(CurrentPlayer)
						L16.state=OldState_L15(CurrentPlayer)
						L115.state=OldState_L117(CurrentPlayer)
						L116.state=OldState_L115(CurrentPlayer)
						L117.state=OldState_L116(CurrentPlayer)
					Else
						L11.state=OldState_L11(CurrentPlayer)
						L12.state=OldState_L12(CurrentPlayer)
						L15.state=OldState_L15(CurrentPlayer)
						L16.state=OldState_L16(CurrentPlayer)
						L115.state=OldState_L115(CurrentPlayer)
						L116.state=OldState_L116(CurrentPlayer)
						L117.state=OldState_L117(CurrentPlayer)
								  
																																							  
																   
					
																																			   
													 
			
									
								  
								  
																																							  
																   
					
																																			   
													   
			
									
								  
								  
																																							  
																   
					 
																																			   
													  
			
											 
			 
			 
				 
																																	
																																	 
					End If
					SaveStateLightturnlight
				End If
			end if
		End If
   
   			
	End If
	ELSE
		If NOT Tilted And Not StopRansom Then
			If keycode = LeftFlipperKey Then 
				helptime.enabled = true	
				If bAttractMode = True Then DMDintroloop:introtime = 0
			End If
			If keycode = RightFlipperKey Then 
				helptime.enabled = true	
				If bAttractMode = True Then DMDintroloop:introtime = 0
			End If
											   
					 
				   
					   
					
		  
				
		   
					  
							   
								
																																						 
							
																									
																				   
																					  
			  
			   
																																   
								
																   
										 
		
					  
					 
		   
		 
		End If
	END IF
	  
End Sub

'************************************** END Table1_KeyDown ************************************
'**********************************************************************************************


	sub wanttoend
		retimct = 1
'		pupDMDDisplay "-","start to restart^flipper to cancel",dmdnote,3,0,20
		pNote "start to resart","flip to cancel"
	end Sub

	sub endthisgame
		EndOfGame()
'		StartAttractMode			'bug with MatchingScore
		canstart=1
		retimct=0
		'vpmtimer.addtimer 4000, "startagain '"
		startagain
'		pupDMDDisplay "-","Quick Restart^Wait 4 Secs",dmdnote,3,0,10
			Plunger.Pullback
			'vpmtimer.addtimer 200, "fireit '"
			fireit
	end Sub

	
	sub startagain
		canstart=0
	end Sub

	Sub Table1_KeyUp(ByVal keycode)


		If keycode = PlungerKey Then
			PlaySoundAt "plunger", Plunger
			Plunger.Fire
		End If

		' Table specific

		If bGameInPLay and hsbModeActive <> True Then

			If keycode = LeftFlipperKey Then
				if PuPGameRunning Then
					PuPGameInfo= PuPlayer.GameUpdate("PupMiniGame", 2 , 83 , "")  'w
				else
					ldown = 0
					If rdown = 0 And ldown = 0 Then
						BallControlTimer.Enabled = True
					End If
					LeftFlipper.RotateToStart
					DOF 101, DOFOff
					PlaySound SoundFX("fx_Flipperdown",DOFFlippers), 0, 1, AudioPan(LeftFlipper), 0.05,0,0,1,AudioFade(LeftFlipper)
				end if
			End If
			If keycode = RightFlipperKey Then
				if PuPGameRunning Then
					PuPGameInfo= PuPlayer.GameUpdate("PupMiniGame", 2 , 87 , "")  'w
				Else
					rdown = 0
					If rdown = 0 And ldown = 0 Then
						BallControlTimer.Enabled = True
					End If
					RightFlipper.RotateToStart:UPRightFlipper.RotateToStart
					DOF 102, DOFOff
					PlaySound SoundFX("fx_Flipperdown",DOFFlippers), 0, 1, AudioPan(RightFlipper), 0.05,0,0,1,AudioFade(RightFlipper)
				end if
			End If
		Else
			If keycode = LeftFlipperKey Then helptime.enabled = false
			If keycode = RightFlipperKey Then helptime.enabled = false
		End If

	End Sub


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'   SHADOWS
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  


	'*****************************************
	'	ninuzzu's	FLIPPER SHADOWS
	'*****************************************

	sub FlipperTimer_Timer()
		FlipperLSh.RotZ = LeftFlipper.currentangle
		FlipperRSh.RotZ = RightFlipper.currentangle
		'FlipperLSh1.RotZ = LeftFlipper1.currentangle

	End Sub

	'*****************************************
	'	ninuzzu's	BALL SHADOW
	'*****************************************
'	Dim BallShadow
'	BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5,BallShadow6,BallShadow7,BallShadow8,BallShadow9,BallShadow10)
'
'	Sub BallShadowUpdate_timer()
'		Dim BOT, b
'		BOT = GetBalls
'		' hide shadow of deleted balls
'		If UBound(BOT)<(tnob-1) Then
'			For b = (UBound(BOT) + 1) to (tnob-1)
'				BallShadow(b).visible = 0
'			Next
'		End If
'		' exit the Sub if no balls on the table
'		If UBound(BOT) = -1 Then Exit Sub
'		' render the shadow for each ball
'		For b = 0 to UBound(BOT)
'			If BOT(b).X < Table1.Width/2 Then
'				BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) + 6
'			Else
'				BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) - 6
'			End If
'			ballShadow(b).Y = BOT(b).Y + 12
'			If BOT(b).Z > 20 Then
'				BallShadow(b).visible = 1
'			Else
'				BallShadow(b).visible = 0
'			End If
'		Next
'	End Sub


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'   TILT
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  


	'NOTE: The TiltDecreaseTimer Subtracts .01 from the "Tilt" variable every round

	Sub CheckTilt                                    'Called when table is nudged
		Tilt = Tilt + TiltSensitivity                'Add to tilt count
		TiltDecreaseTimer.Enabled = True


'	If CurrentMissionFlag(CurrentPlayer) = 0 Then
		Select Case Int(Rnd*22)+1
			Case 1 : audioknight = "Sound-0x01B0.mp3":SpeakTime = 2526	'Super Feats is Boosted
			Case 2 : audioknight = "Sound-0x017C.mp3":SpeakTime = 1083	'Super Feats is Boosted
			Case 3 : audioknight = "Sound-0x01F0.mp3":SpeakTime = 2588 	'Super Feats is Boosted
			Case 4 : audioknight = "Sound-0x02C6.mp3":SpeakTime = 1119	'Super Feats is Boosted
			Case 5 : audioknight = "Sound-0x03B3.mp3":SpeakTime = 1757	'Super Feats is Boosted
			Case 6 : audioknight = "Sound-0x03E5.mp3":SpeakTime = 1078 	'Super Feats is Boosted
			Case 7 : audioknight = "Sound-0x03F0.mp3":SpeakTime = 1634	'Super Feats is Boosted
			Case 8 : audioknight = "Sound-0x0461.mp3":SpeakTime = 2046	'Super Feats is Boosted
			Case 9 : audioknight = "Sound-0x0417.mp3":SpeakTime = 1718 	'Super Feats is Boosted
			Case 10 : audioknight = "Sound-0x0186.mp3":SpeakTime = 1527	'Super Feats is Boosted
			Case 11 : audioknight = "Sound-0x0249.mp3":SpeakTime = 1751	'Super Feats is Boosted
			Case 12 : audioknight = "Sound-0x0381.mp3":SpeakTime = 1950	'Super Feats is Boosted
			Case 13 : audioknight = "Sound-0x03A9.mp3":SpeakTime = 2260	'Super Feats is Boosted
			Case 14 : audioknight = "Sound-0x03B3.mp3":SpeakTime = 1757	'Super Feats is Boosted
			Case 15 : audioknight = "Sound-0x0480.mp3":SpeakTime = 2316	'Super Feats is Boosted
			Case 16 : audioknight = "Sound-0x01B8.mp3":SpeakTime = 2436	'Super Feats is Boosted
			Case 17 : audioknight = "Sound-0x023D.mp3":SpeakTime = 1903	'Super Feats is Boosted
			Case 18 : audioknight = "Sound-0x02B9.mp3":SpeakTime = 1475	'Super Feats is Boosted
			Case 19 : audioknight = "Sound-0x03D8.mp3":SpeakTime = 1117	'Super Feats is Boosted
			Case 20 : audioknight = "Sound-0x0271.mp3":SpeakTime = 2103	'Super Feats is Boosted
			Case 21 : audioknight = "Sound-0x040f.mp3":SpeakTime = 2312	'Super Feats is Boosted
			Case 22 : audioknight = "Sound-0x041A.mp3":SpeakTime = 2237	'Super Feats is Boosted
		End Select
		LightEyesBK
		playmedia audioknight,"audioknight",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	End If


		If(Tilt> TiltSensitivity) AND(Tilt <15) Then 'show a warning
			If RetroMode = 1 or RetroMode = 3 Then
				'playmedia "Sound-0x0047.mp3","Audionoise",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			Else
				pNote "CAREFUL!","TILT WARNING"
				DOF 302, DOFPulse ' Tilt Warning
'				pupDMDDisplay "-","Tilt^Warning!",dmdnote,3,0,10
				'PlaySound "buzz"			
'				PuPlayer.playlistplayex pBackglass,"videotilt","",100,1
'				playmedia "Danger.mp4","Danger",pBackglass,"",3000,"",1,1  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				Video_Danger
			'	playmedia "Sound-0x01B0.mp3","Audioknight",pAudio,"",0,"",1,10  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)

			End If
		End if
		If Tilt> 15 Then 'If more that 15 then TILT the table
			Tilted = True
			DrainHited.Enabled = True
			UnblockDrainHited.Enabled = True
			pNote "TILT",""
'			pupDMDDisplay "-","TILT^TILT",dmdnote,3,0,10
				'PlaySound "powerdownn"
			'playmedia "brilliant that makes me feel loads better.mp4","videodrains",pBackglass,"",3000,"",1,1  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			If RetroMode = 1 or RetroMode = 3 Then
				playmedia "Sound-0x0149.mp3","Audionoise",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			Else
'				playmedia "Tilt.mp4","Tilt",pBackglass,"",3000,"",1,1  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				Video_Tilt
			End If
'			'DOF 310, DOFPulse   'DOF MX - TILT
			DOF 301, DOFPulse 'Tilt
			DisableTable True
			tilttime = 0
			tilttableclear.enabled = true
			TiltRecoveryTimer.Enabled = True 'start the Tilt delay to check for all the balls to be drained
		End If
	End Sub

Sub StopPlayingGame
	DisableTable True
	tilttableclear.enabled = true
	StopBk2000 = 1
End Sub

	sub tilttableclear_timer
		tilttime = tilttime + 1
		Select Case tilttime
			Case 10
				tableclearing
		End Select
	End Sub

	Sub tableclearing

	End Sub

	Sub posttiltreset

	End Sub

	Sub TiltDecreaseTimer_Timer
		If Tilt> 0 Then
			Tilt = Tilt - 0.1
		Else
			TiltDecreaseTimer.Enabled = False
		End If
	End Sub

	Sub DisableTable(Enabled)
		If Enabled Then
			GiOff
'			LightSeqTilt.Play SeqAllOff
			LeftFlipper.RotateToStart
			UPRightFlipper.RotateToStart
			RightFlipper.RotateToStart
			LeftSlingshot.Disabled = 1
			RightSlingshot.Disabled = 1
			'PuPlayer.playresume 4
			playbgaudio
			playclear pAudio
			playclear pMusic
		Else
			GiOn
'			LightSeqTilt.StopPlay
			LeftSlingshot.Disabled = 0
			RightSlingshot.Disabled = 0
		End If
	End Sub

	Sub TiltRecoveryTimer_Timer()
		If(BallsOnPlayfield = 0) Then
			
			EndOfBall()
			TiltRecoveryTimer.Enabled = False
		End If
	End Sub



'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'   SOUND FUNCTIONS
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  


'***************************************************************
'             Supporting Ball & Sound Functions v3.0
'  includes random pitch in PlaySoundAt and PlaySoundAtBall
'***************************************************************



TableWidth = Table1.width
TableHeight = Table1.height

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / TableWidth-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10))
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / table1.width-1
    If tmp > 0 Then
        AudioPan = Csng(tmp ^10)
    Else
        AudioPan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = (SQR((ball.VelX ^2) + (ball.VelY ^2)))
End Function

Function AudioFade(ball) 'only on VPX 10.4 and newer
    Dim tmp
    tmp = ball.y * 2 / TableHeight-1
    If tmp > 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10))
    End If
End Function

																																  
																			   
	   

'Sub PlaySoundAt(soundname, tableobj) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
'    PlaySound soundname, 0, 1, Pan(tableobj), 0.1, 0, 0, 0, AudioFade(tableobj)
'End Sub
'
'Sub PlaySoundAtBall(soundname) ' play a sound at the ball position, like rubbers, targets, metals, plastics
'    PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0.4, 0, 0, 0, AudioFade(ActiveBall)
'End Sub
'*********************************************************************
'                 Positional Sound Playback Functions
'*********************************************************************

' Play a sound, depending on the X,Y position of the table element (especially cool for surround speaker setups, otherwise stereo panning only)
' parameters (defaults): loopcount (1), volume (1), randompitch (0), pitch (0), useexisting (0), restart (1))
' Note that this will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position
Sub PlayXYSound(soundname, tableobj, loopcount, volume, randompitch, pitch, useexisting, restart)
	PlaySound soundname, loopcount, volume, AudioPan(tableobj), randompitch, pitch, useexisting, restart, AudioFade(tableobj)
End Sub

' Similar subroutines that are less complicated to use (e.g. simply use standard parameters for the PlaySound call)
Sub PlaySoundAt(soundname, tableobj)
    PlaySound soundname, 1, 1, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtVol(soundname, tableobj, vol)
    PlaySound soundname, 1, (vol), AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
    PlaySoundAt soundname, ActiveBall
End Sub

'***********************************************
'   JP's VP10 Rolling Sounds + Ballshadow v3.0
'   uses a collection of shadows, aBallShadow
'***********************************************

Const tnob = 19   'total number of balls, 20 balls, from 0 to 19
Const lob = 0     'number of locked balls
Const maxvel = 50 'max ball velocity 25-50
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingSoundUpdate()
    Dim BOT, b, ballpitch, ballvol, speedfactorx, speedfactory
    BOT = GetBalls

    ' stop the sound of deleted balls and hide the shadow
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
        aBallShadow(b).Y = 3000
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' play the rolling sound for each ball and draw the shadow
    For b = lob to UBound(BOT)
        aBallShadow(b).X = BOT(b).X
        aBallShadow(b).Y = BOT(b).Y

        If BallVel(BOT(b)) > 1 Then
            If BOT(b).z < 30 Then
                ballpitch = Pitch(BOT(b))
                ballvol = Vol(BOT(b))
            Else
                ballpitch = Pitch(BOT(b)) + 25000 'increase the pitch on a ramp
                ballvol = Vol(BOT(b)) * 10
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, ballvol, Pan(BOT(b)), 0, ballpitch, 1, 0, AudioFade(BOT(b))
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
               rolling(b) = False
            End If
        End If

        ' rothbauerw's Dropping Sounds
        If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_ball_drop1", 0, ABS(BOT(b).velz) / 17, Pan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
	        End If

        ' jps ball speed control									
'        If BOT(b).VelX AND BOT(b).VelY <> 0 Then						' This part is deactived because the "CastleVUK2" is impacted --> The launch of the ball is not constant and the ball does not reach the upper rightflipper.
'            speedfactorx = ABS(maxvel / BOT(b).VelX)
'            speedfactory = ABS(maxvel / BOT(b).VelY)
'            If speedfactorx < 1 Then
'                BOT(b).VelX = BOT(b).VelX * speedfactorx
'                BOT(b).VelY = BOT(b).VelY * speedfactorx
'            End If
'            If speedfactory < 1 Then
'                BOT(b).VelX = BOT(b).VelX * speedfactory
'                BOT(b).VelY = BOT(b).VelY * speedfactory
'            End If
'       End If
    Next
End Sub


'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0
End Sub

'******************************
' Diverse Collection Hit Sounds
'******************************

'Sub LeftFlipper_Collide(parm)
' 	RandomSoundFlipper()
'	TextBox1.text = "TouchL"
'End Sub
'
'Sub RightFlipper_Collide(parm)
' 	RandomSoundFlipper()
'	TextBox1.text = "TouchR"
'End Sub
'
'Sub RandomSoundFlipper()
'ValueBox2.text = "Touch"
'	Select Case Int(Rnd*3)+1
'		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
'		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
'		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
'	End Select
'End Sub

'Sub aTargets_Hit(idx):PlaySoundatBall "fx_target":End Sub
'Sub aBumpers_Hit (idx): PlaySoundatball SoundFX("fx_bumper", DOFContactors) : End Sub 
'Sub aRollovers_Hit(idx):PlaySoundatball "fx_sensor":End Sub
'Sub aGates_Hit(idx):PlaySoundatball "fx_Gate":End Sub
'Sub aMetals_Hit(idx):PlaySoundatball "fx_MetalHit2":End Sub
Sub aRubber_Bands_Hit(index):PlaySoundatball "fx_rubber2":End Sub
Sub aRubber_Pegs_Hit(index):PlaySoundatball "fx_rubber2":End Sub
Sub KickerVUK_Hit(Index):PlaySoundatball "fx_kicker-enter":End Sub
Sub aSensors_Hit(Index):PlaySoundatball "fx_sensor":End Sub
'Sub aRubber_Posts_Hit(idx):PlaySoundatball "fx_postrubber":End Sub
'Sub aRubber_Pins_Hit(idx):PlaySoundatball "fx_postrubber":End Sub
'Sub aYellowPins_Hit(idx):PlaySoundatball "fx_postrubber":End Sub
'Sub aPlastics_Hit(idx):PlaySoundatball "fx_PlasticHit":End Sub
'Sub aWoods_Hit(idx):PlaySoundatball "fx_Woodhit":End Sub

' Ramp Soundss
Sub RHelp1_Hit()
    StopSound "fx_metalrolling"
    PlaySound "fx_ballrampdrop", 0, 1, pan(ActiveBall)
End Sub

Sub RHelp2_UnHit()
    StopSound "fx_metalrolling"
    'PlaySound "fx_balldrop", 0, 1, pan(ActiveBall)
    PlaySoundAt "fx_balldrop",sw66_sub
End Sub

'*****************************************
'	Ball Shadow
'*****************************************

'Dim BallShadow
'BallShadow = Array (BallShadow1, BallShadow2, BallShadow3, BallShadow4, BallShadow5)
'
'
'Sub BallShadowUpdate()
'    Dim BOT, b
'    BOT = GetBalls
'
'	' render the shadow for each ball
'    For b = 0 to UBound(BOT)
'		If BOT(b).X < Table1.Width/2 Then
'			BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) + 10
'		Else
'			BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) - 10
'		End If
'
'			BallShadow(b).Y = BOT(b).Y + 10
'			BallShadow(b).Z = 1
'		If BOT(b).Z > 20 AND shad_off = 0 Then
'			BallShadow(b).visible = 1
'		Else
'			BallShadow(b).visible = 0
'		End If
'	Next
'End Sub

Sub Shadow_Off_Hit()
shad_off = 1
End Sub

Sub Shadow_Off_Unhit()
shad_off = 0
End Sub


Sub RollingTimer_Timer()
'	BallShadowUpdate
	RollingSoundUpdate
'	FlipperLSh.RotZ = LeftFlipper.currentangle
'	FlipperRSh.RotZ = RightFlipper.currentangle
'	FlipperRTop.RotZ = RightFlipper2.currentangle
End Sub

	'**************************************
	' Explanation of the collision routine
	'**************************************

	' The collision is built in VP.
	' You only need to add a Sub OnBallBallCollision(ball1, ball2, velocity) and when two balls collide they 
	' will call this routine. What you add in the sub is up to you. As an example is a simple Playsound with volume and paning
	' depending of the speed of the collision.


	Sub Pins_Hit (idx)
		PlaySound "pinhit_low", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
	End Sub

	Sub triggers_Hit (idx)
		'debug.print "triggers"
		PlaySound "fx_rollover", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
	End Sub

	Sub Targets_Hit (idx)
		PlaySound "target", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
	End Sub

	Sub Metals_Thin_Hit (idx)
		PlaySound "metalhit_thin", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Sub

	Sub Metals_Medium_Hit (idx)
		PlaySound "metalhit_medium", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Sub

	Sub Wall031_Hit
	'	PlaySound "metalhit2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		PlaySound "metalhit2"
	End Sub

	Sub Gates_Hit (idx)
		PlaySound "gate4", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Sub

	Sub Spinner_Spin
		PlaySound "fx_spinner", 0, .25, AudioPan(Spinner), 0.25, 0, 0, 1, AudioFade(Spinner)
	End Sub

	Sub Rubbers_Hit(idx)
		'debug.print "rubber"
		dim finalspeed
		finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
		If finalspeed > 20 then 
			PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		End if
		If finalspeed >= 6 AND finalspeed <= 20 then
			RandomSoundRubber()
		End If
	End Sub

	Sub Posts_Hit(idx)
		dim finalspeed
		finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
		If finalspeed > 16 then 
			PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		End if
		If finalspeed >= 6 AND finalspeed <= 16 then
			RandomSoundRubber()
		End If
	End Sub

	Sub RandomSoundRubber()
		Select Case Int(Rnd*3)+1
			Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
			Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
			Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		End Select
	End Sub

	Sub LeftFlipper_Collide(parm)
		RandomSoundFlipper()
	End Sub

	Sub RightFlipper_Collide(parm)
		RandomSoundFlipper()
	End Sub

Sub UPRightFlipper_Collide(parm)
	RandomSoundFlipper()
End Sub

	Sub RandomSoundFlipper()
		Select Case Int(Rnd*3)+1
			Case 1 : PlaySound "flip_hit_1" ', 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
			Case 2 : PlaySound "flip_hit_2" ', 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
			Case 3 : PlaySound "flip_hit_3" ', 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		End Select
	End Sub

	Sub PlasticRampBumps_Hit(idx)
		'debug.print "plasticrampbump"
		if BallVel(ActiveBall) > .3 and Timer > NextOrbitHit then
			RandomBump 10, -20000
			' Schedule the next possible sound time.  This prevents it from rapid-firing noises too much. 
			' Lowering these numbers allow more closely-spaced clunks.
			NextOrbitHit = Timer + .1 + (Rnd * .2)
		end if 
	End Sub

	Sub PlasticBumps_Hit(idx)
		'debug.print "plasticbump"
		PlaySoundAtBall "fx_plastichit"
	End Sub

	Sub MetalWallBumps_Hit(idx)
		'debug.print "MetalWall"
		if BallVel(ActiveBall) > .3 and Timer > NextOrbitHit then
			RandomBump 3, 20000 'Increased pitch to simulate metal wall
			' Schedule the next possible sound time.  This prevents it from rapid-firing noises too much. 
			' Lowering these numbers allow more closely-spaced clunks.
			NextOrbitHit = Timer + .2 + (Rnd * .2)
		end if 
	End Sub

	Sub WireRampBumps_Hit(idx)
		'debug.print "wirebump"
		if BallVel(ActiveBall) > .3 and Timer > NextOrbitHit then
			dim BumpSnd:BumpSnd= "wirerampbump" & CStr(Int(Rnd*5)+1)
			PlaySound BumpSnd, 0, Vol(ActiveBall) * .5, Pan(ActiveBall), 0, 30000, 0, 1, AudioFade(ActiveBall)
			NextOrbitHit = Timer + .2 + (Rnd * .2)
		end if 
	End Sub

'	Sub RandomBump(voladj, freq)
'		'debug.print "randombump"
'		dim BumpSnd:BumpSnd= "rampbump" & CStr(Int(Rnd*7)+1)
'		PlaySound BumpSnd, 0, Vol(ActiveBall)*voladj, Pan(ActiveBall), 0, freq, 0, 1, AudioFade(ActiveBall)
'	End Sub

	' Requires rampbump1 to 7 in Sound Manager
	Sub RandomBump(voladj, freq)
		dim BumpSnd:BumpSnd= "rampbump" & CStr(Int(Rnd*7)+1)
		PlaySound BumpSnd, 0, Vol(ActiveBall)*voladj, Pan(ActiveBall), 0, freq, 0, 1, AudioFade(ActiveBall)
	End Sub

	

	' Stop Bump Sounds
	Sub BumpSTOPMetal ()
		'debug.print "bumpstopmetal"
	dim i:for i=1 to 7:StopSound "RampBump" & i:next
	NextOrbitHit = Timer + 1
	End Sub

	Sub BumpSTOPWire ()
		'debug.print "bumpstopwire"
	dim i:for i=1 to 4:StopSound "WireRampBump" & i:next
	NextOrbitHit = Timer + 1
	End Sub

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'   START GAME, END GAME
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  

Sub ResetForNewGame()
LastChanceCatapultMode = False 
'compteuri = 901
'Fireanimation.Enabled = True
	WarHurryUp51Touch=0
	WarHurryUp56Touch=0
	WarHurryUp58Touch=0
	BaseBonus = 0
	SuperFeatures = 0
	Combos = 0
	Loops = 0
'	EndOfGameFlag = False
' 	SCORE rules
	AddScoreLoopBonus = 500000
	AddScoreSW82 = 1000000
	AddscoreCatapult = 0
'	End SCORE rules	
	Gamerover_flag = False
	Dim i
	StopAttractMode
'	InProgress=True
	bGameInPLay = True
	'GiOn
	TotalGamesPlayed = TotalGamesPlayed + 1
	savegp
	CurrentPlayer = 1
	PlayersPlayingGame = 1
	BallsOnPlayfield = 0
	bOnTheFirstBall = True
	StopBk2000 = 0
	lightstateflag=1
	StopRansom = False
	SeqAnimation=0
	TurnOffRoundLights
	Knight_challenge_flag = False
	KnightRemaining = 3
	L11.state=0
	L12.state=0
	L15.state=0
	L16.state=0
	L115.state=0
	L116.state=0
	L117.state=0
	L131.state=0
	L132.state=0
	L133.state=0
	RampLightsMove(CurrentPlayer) = "Blinking"
	CheckLamp.Interval=100
	CheckLamp.Enabled = True
	GoToInitVideo.Enabled=False
	StopBouleFlag = False
	StopRotating.interval=5000
	StopBouleAngle = 0
	StopRotating.Enabled=True
'     InProgress=True
'	 DrainTimer.Interval=2500
 '    DrainTimer.Enabled = True
'	Ball=0
	BallInLocker = 0
	WickedCavernLeftOrRight = ""
	WickedCavernLeftOrRightOld = ""
	DeepFreezeTargetRedHit = False
		For i = 1 To MaxPlayers
			L66color(i) = "white"
			L72color(i) = "white"
			L79color(i) = "white"
			L85color(i) = "white"
			L91color(i) = "white"
			L98color(i) = "white"
			L103color(i) = "white"
			MagnaSaveFlag(i) = 1
			RemainingHit(i) = 0
			RampChangeStatus(i) = 0
			KnightLamp(i) = 4			'default 4
'			KnightLamp(i) = 12			'default 4
			
			BallInCatapult(i) = 0
			BlackKnightRetro(i) = 0		'default 0
'			BlackKnightRetro(i) = 2		'default 0

			Score(i) = 0
			BonusPoints(i) = 0
			BonusHeldPoints(i) = 0
			BonusMultiplier(i) = 1
			BallsRemaining(i) = bpgcurrent
			ExtraBallsAwards(i) = 0
			BallSaveAvailable(i) = True
			Knight_challenge(i) = 0
			MudBog(i)=0
			MoltenFire(i)=0
			BurningSands(i)=0
			WickedCavern(i)=0
			DeepFreeze(i)=0
			BlackCastle(i)= 0
			MudBogDefeated(i)= 0
			MoltenFireDefeated(i)= 0
			BurningSandsDefeated(i)= 0
			WickedCavernDefeated(i)= 0
			DeepFreezeDefeated(i)= 0
			BlackCastleDefeated(i)= 0
			catapult_lock_is_lit(i) = True
			RampActivForValidateMission(i) = False
			ShieldIsReadyToActivateMission(i) = False
			NumberOfHitForStartMission(i) = 0
			NumberOfMissioncomplete(i) = 0
			CountSW45used(i) = 0
			RightGateCollidable(i)=True
			L66State(i) = 0
			L72State(i) = 0
			L79State(i) = 0
			L85State(i) = 0
			L91State(i) = 0
			L98State(i) = 0
			L103State(i) = 0
			RandomL01(i) = ""
			RandomL02(i) = ""
			RandomL03(i) = ""
			RandomR01(i) = ""
			RandomR02(i) = ""
			RandomR03(i) = ""
			DeepFreezeMissionHitStart(i) = False
			BlackCastleMissionHitStart(i) = False
			AddScoreMudBogTotal(i) = 0
			AddScoreMoltenFireTotal(i) = 0
			AddScoreDeepFreezeTotal(i) = 0
			AddScoreSandWormTotal(i) = 0
			AddScoreWickedCavernTotal(i) = 0
			AddScoreBlackCastleTotal(i) = 0
			AddscoreCatapultTotal(i) = 0
			AddScoreTripleKnightChallengeTotal(i) = 0
			AddScoreWarHurryUpTotal(i) = 0
			Catapult_Mode_Total_score(i) = 0
			LastChanceChronoStart(i) = False
			ReplayPlayer(i) = False
			AddScoreMudBogTotalEndGame(i) = 0
			AddScoreMoltenFireTotalEndGame(i) = 0
			AddScoreSandWormTotalEndGame(i) = 0
			AddScoreWickedCavernTotalEndGame(i) = 0
			AddScoreDeepFreezeTotalEndGame(i) = 0
			AddScoreBlackCastleTotalEndGame(i) = 0
			AddscoreCatapultTotalEndGame(i) = 0
			AddScoreTripleKnightChallengeTotalEndGame(i) = 0
			AddScoreWarHurryUpTotalEndGame(i) = 0
			ExtraBallAfterMonsterDefeated(i) = ExtraBallAfterMonsterDefeatedValue
			'***************** Variable used for Check the HighScore ******************************
			ScoreComboChampion(i) = 0
			ScoreKnightChampion(i) = 0
			ScoreBlackCastleChampion(i) = 0
			ScoreLoopChampion(i) = 0
			ScoreWarChampion(i) = 0
			ScoreBonusChampion(i) = 0
			ScoreTripleKnightsMBChampion(i) = 0
			ScoreCatapultMBChampion(i) = 0
			ScoreMoltenFireChampion(i) = 0
			ScoreDeepFreezeChampion(i) = 0
			ScoreMudBogChampion(i) = 0
			ScoreWickedCavernChampion(i) = 0
			ScoreBurningSandsChampion(i) = 0
			ScoreSuperChampion(i) = 0
			CurrentMissionFlag(i) = 0
		Next
		LastChanceFlag = False
		LastChanceStart = False
		Tilt = 0
		BouleHited = 0
		Game_Init()
		pDMDStartGame
		ShieldRemainsLock = False
		BallInLocker = 0
		'vpmtimer.addtimer 1500, "FirstBall '"		
		'FirstBall
		FirstBall.Enabled=True
		ShieldNumberOfMove = 4
		ShieldTimer.interval=250
		ShieldTimer.Enabled=True
		UPLock.collidable=True
		UPLock.TransY=0
		boule.rotx=0
'		PlaySound "Sound-0x0111",-1
		PlaySound "Sound-0x0111", 0, SongVolume
		Timer2.Enabled=True
		ColorRoundLight = 0
		MissionRandomTimer.Interval=500
		MissionRandomTimer.enabled=True
		SelectMission		

		BeginCurrentMissionFlag = 0
		Tilt = 0
		CheckLampAction.Enabled=True

	End Sub


Sub EndOfGame()
	UPLock.collidable=False
	UPLock.TransY=-20
	BallControlTimer.Enabled = False
	BallKeptInLocker =  0
	LockerTimer.Enabled=True
	CheckLamp.Enabled=False
	CheckLampAction.Enabled=False
	horloge.Enabled=False
	MissionRandomTimer.enabled=False
	TurnOffRoundLights
	LeftFlipper.RotateToStart:UPRightFlipper.RotateToStart:RightFlipper.RotateToStart
	Video_Display_Match.Interval=2500
	Video_Display_Match.Enabled=True
	playclear pMusic
	Gamerover_flag = True
	Table1_MusicDone
	pNote "GAME OVER","PLAY AGAIN"
	PlayersPlayingGame = 0
	PuPlayer.LabelSet pBackglass,"HighScore","",1,""
	PuPlayer.LabelSet pBackglass,"HighScore2","",1,""
	PuPlayer.LabelSet pBackglass,"HighScore2","",1,""
	PuPlayer.LabelSet pBackglass,"HighScoreL1","",1,""
	PuPlayer.LabelSet pBackglass,"HighScoreL2","",1,""
	PuPlayer.LabelSet pBackglass,"HighScoreL3","",1,""
	PuPlayer.LabelSet pBackglass,"high1name","",1,""
	PuPlayer.LabelSet pBackglass,"high1score","",1,""
	PuPlayer.LabelSet pBackglass,"high2name","",1,""
	PuPlayer.LabelSet pBackglass,"high2score","",1,""
	PuPlayer.LabelSet pBackglass,"high3name","",1,""
	PuPlayer.LabelSet pBackglass,"high3score","",1,""
	PuPlayer.LabelSet pBackglass,"high4name","",1,""
	PuPlayer.LabelSet pBackglass,"high4score","",1,""
	PuPlayer.LabelSet pBackglass,"high5name","",1,""
	PuPlayer.LabelSet pBackglass,"high5score","",1,""
	gameovervids
	pDMDGameOver
	introposition = 0
	bGameInPLay = False
	bJustStarted = False
	hsbModeActive = False
	Dim i
	GiOff
	helpful.enabled = 0
End Sub

	sub gameovervids
		GiOff
		StartLightSeq
		cineon = 1
		StartRainbow alights
		StartRainbow GI
		DMDattract.Enabled = 1
		'bAttractMode = True
		dim govdnum:govdnum=RndNum(1,2)
		select case govdnum
			case 1
'				playmedia "dark and difficult times lie ahead.mp4","videogo",pBackglass,"",0,"",1,9
				'vpmtimer.addtimer 14000, "StartAttractMode() '"
'				StartAttractMode()		'bug with MatchingScore
			case 2
'				playmedia "everythings gonna change now isnt it.mp4","videogo",pBackglass,"",0,"",1,9
				'vpmtimer.addtimer 14000, "StartAttractMode() '"
'				StartAttractMode() 'bug with MatchingScore
		end Select
	end Sub


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'   Pinup Active Backglass
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  

'**************************
'   PinUp Player Config
'   Change HasPuP = True if using PinUp Player Videos
'**************************

'	Dim HasPup:HasPuP = True
'
'	Dim PuPlayer
'
'	Const pTopper=0
'	Const pDMD=11
'	Const pBackglass=1
''	Const pDMD=1
''	Const pBackglass=2
'	Const pPlayfield=3
'	Const pMusic=4
'	Const pAudio=5
'	Const pCallouts=6
'	Const pBackGlass2=7
'	Const pPopSW71=8
'	Const pPopSW72=9
'	Const pPop=10
'	Const pOvervid=14
'	Const pGame=15


	if HasPuP Then
	on error resume next
	Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay") 
	PuPlayer.B2SInit "", pGameName   'use new method to startup pup with dmd via puppack
	on error goto 0
	if not IsObject(PuPlayer) then HasPuP = False
	end If

	
	PuPlayer.LabelInit pBackglass

	'Setup Pages.  Note if you use fonts they must be in FONTS folder of the pupVideos\tablename\FONTS
	'syntax - PuPlayer.LabelNew <screen# or pDMD>,<Labelname>,<fontName>,<size%>,<colour>,<rotation>,<xAlign>,<yAlign>,<xpos>,<ypos>,<PageNum>,<visible>

	'Page 1 (default score display)
	'colors
	'dark - 2697513
	'yellow - 2477823
	'light - 15066597

	'top
	PuPlayer.LabelNew pBackglass,"potionnum",typefont,			3,15066597  ,0,0,1,23,81,1,0
	PuPlayer.LabelNew pBackglass,"potionname",typefont,			3,15066597  ,0,0,1,23,81,1,0
	PuPlayer.LabelNew pBackglass,"egghits",typefont,			3,2477823  	,0,0,1,23,81,1,0
	PuPlayer.LabelNew pBackglass,"egglocks",typefont,			3,2477823  	,0,0,1,23,81,1,0
	PuPlayer.LabelNew pBackglass,"lakespins",typefont,			3,2477823  	,0,0,1,23,81,1,0
	PuPlayer.LabelNew pBackglass,"lakejack",typefont,			3,2477823  	,0,0,1,23,81,1,0
	PuPlayer.LabelNew pBackglass,"mazeramp",typefont,			3,2477823  	,0,0,1,23,81,1,0
	PuPlayer.LabelNew pBackglass,"mazejacks",typefont,			3,2477823  	,0,0,1,23,81,1,0
	PuPlayer.LabelNew pBackglass,"spellname",typefont,			3,15066597  ,0,0,1,23,81,1,0
	PuPlayer.LabelNew pBackglass,"spellnum",typefont,			3,15066597  ,0,0,1,23,81,1,0
	PuPlayer.LabelNew pBackglass,"spellnum2",typefont2,			3,15066597  ,0,0,1,23,81,1,0
	'bottom

	PuPlayer.LabelNew pBackglass,"gobletnum",typefont,			3,2477823  	,0,0,1,23,81,1,0
	PuPlayer.LabelNew pBackglass,"deatheaternum",typefont,		3,2477823  	,0,0,1,23,81,1,0
	PuPlayer.LabelNew pBackglass,"modenum",typefont,			3,2477823  	,0,0,1,23,81,1,0
	PuPlayer.LabelNew pBackglass,"combonum",typefont,			3,2477823  	,0,0,1,23,81,1,0
	PuPlayer.LabelNew pBackglass,"modetimernum",typefont,		3,2477823  	,0,0,1,23,81,1,0
	PuPlayer.LabelNew pBackglass,"Ball",typefont,				2,2477823 	,0,1,1,50,81,1,1
	PuPlayer.LabelNew pBackglass,"Ball2",typefont2,				2,2477823 	,0,1,1,50,81,1,1	
	PuPlayer.LabelNew pBackglass,"curplayer",typefont,			3,2477823	,0,1,1,50,93,1,1
	PuPlayer.LabelNew pBackglass,"curscore",numberfont,			7,2477823	,0,1,1,50,87,1,1
	PuPlayer.LabelNew pBackglass,"curscore2",numberfont2,		7,2477823	,0,1,1,50,87,1,1
	PuPlayer.LabelNew pBackglass,"curscore3",numberfont2,		7,2477823	,0,1,1,50,87,1,1

	PuPlayer.LabelNew pBackglass,"TitleBonus",numberfont,		7,2477823	,0,1,1,50,87,1,1
	PuPlayer.LabelNew pBackglass,"TextBaseBonus",numberfont,		7,2477823	,0,1,1,50,87,1,1
	PuPlayer.LabelNew pBackglass,"TextSuperFeatures",numberfont,	7,2477823	,0,1,1,50,87,1,1
	PuPlayer.LabelNew pBackglass,"TextCombos",numberfont,			7,2477823	,0,1,1,50,87,1,1
	PuPlayer.LabelNew pBackglass,"TextLoops",numberfont,			7,2477823	,0,1,1,50,87,1,1
	PuPlayer.LabelNew pBackglass,"TextTotalBonus",numberfont,		7,2477823	,0,1,1,50,87,1,1

	PuPlayer.LabelNew pBackglass,"BaseBonus",numberfont,		7,2477823	,0,1,1,50,87,1,1
	PuPlayer.LabelNew pBackglass,"SuperFeatures",numberfont,	7,2477823	,0,1,1,50,87,1,1
	PuPlayer.LabelNew pBackglass,"Combos",numberfont,			7,2477823	,0,1,1,50,87,1,1
	PuPlayer.LabelNew pBackglass,"Loops",numberfont,			7,2477823	,0,1,1,50,87,1,1
	PuPlayer.LabelNew pBackglass,"BonusMultiplier",numberfont,	7,2477823	,0,1,1,50,87,1,1
	PuPlayer.LabelNew pBackglass,"TotalBonus",numberfont,		7,2477823	,0,1,1,50,87,1,1
	PuPlayer.LabelNew pBackglass,"TotalScoreBonus",numberfont,	7,2477823	,0,1,1,50,87,1,1

	PuPlayer.LabelNew pBackglass,"AddScoreDisplayed",typefontbold,	7,2477823	,0,1,1,50,87,1,1	
	PuPlayer.LabelNew pBackglass,"AddScoreDisplayed2",numberfont,	7,2477823	,0,1,1,50,87,1,1	
	PuPlayer.LabelNew pBackglass,"CommentDisplayed",typefontbold,	10,2477823	,0,1,1,50,87,1,1	
	PuPlayer.LabelNew pBackglass,"CommentDisplayed2",numberfont,	10,2477823	,0,1,1,50,35,1,1	

	PuPlayer.LabelNew pBackglass,"Mystery",numberfont,			10,30719	,0,1,1,50,87,1,1	
	PuPlayer.LabelNew pBackglass,"RandomMystery",numberfont,	10,30719	,0,1,1,50,35,1,1


	PuPlayer.LabelNew pBackglass,"LastChanceBallToLock",numberfont,	10,16440832	,0,1,1,50,25,1,1	
	PuPlayer.LabelNew pBackglass,"LastChanceTime",typefont2,		30,255	,0,1,1,50,50,1,1
	PuPlayer.LabelNew pBackglass,"LastChanceJackpot",numberfont,	10,16440832	,0,1,1,50,75,1,1	

	PuPlayer.LabelNew pBackglass,"Play1score",numberfont,		3,2477823  	,0,0,1,31,81,1,0
	PuPlayer.LabelNew pBackglass,"Play2score",numberfont,		3,2477823  	,0,0,1,31,85,1,0
	PuPlayer.LabelNew pBackglass,"Play3score",numberfont,		3,2477823  	,0,0,1,31,89,1,0
	PuPlayer.LabelNew pBackglass,"Play4score",numberfont,		3,2477823  	,0,0,1,31,93,1,0
	PuPlayer.LabelNew pBackglass,"ruletitle",numberfont,		3,15066597  ,0,1,1,50,12,1,1
	PuPlayer.LabelNew pBackglass,"rulecopy1",typefont,			2,15066597 	,0,1,1,50,15,1,1
	PuPlayer.LabelNew pBackglass,"rulecopy2",typefont,			2,15066597 	,0,1,1,50,15,1,1


	
	'onverlay
	PuPlayer.LabelNew pBackglass,"titleimg",zoomfont,			6,16777215 	,0,1,1, 0,0,1,1
	PuPlayer.LabelSet pBackglass,"titleimg","DMDImages\\popframe.png",1,"{'mt':2,'width': 0, 'height': 0, 'yalign': 0}"
	PuPlayer.LabelNew pBackglass,"titlebg",zoombgfont,			9,0  		,0,1,1,50,50,1,1
	PuPlayer.LabelNew pBackglass,"title",zoomfont,				9,16777215 	,0,1,1,50,50,1,1
	PuPlayer.LabelNew pBackglass,"titlebg2",zoombgfont,			6,0  		,0,1,1,50,50,1,1
	PuPlayer.LabelNew pBackglass,"title2",zoomfont,				6,16777215 	,0,1,1,50,50,1,1
	PuPlayer.LabelNew pBackglass,"mgscore",numberfont,			9,15066597	,0,1,1,50,47,1,1
	PuPlayer.LabelNew pBackglass,"matchscore",numberfont,		15,15066597	,0,1,1,50,25,1,1

	'attract
	PuPlayer.LabelNew pBackglass,"high1name",typefont,			5,16777215  ,0,1,1,22,30,1,1
	PuPlayer.LabelNew pBackglass,"high1score",numberfont,		5,16777215  ,0,1,1,36,30,1,1
	PuPlayer.LabelNew pBackglass,"high2name",typefont,			5,16777215  ,0,1,1,22,38,1,1
	PuPlayer.LabelNew pBackglass,"high2score",numberfont,		5,16777215  ,0,1,1,36,38,1,1
	PuPlayer.LabelNew pBackglass,"high3name",typefont,			5,16777215  ,0,1,1,22,46,1,1
	PuPlayer.LabelNew pBackglass,"high3score",numberfont,		5,16777215  ,0,1,1,36,46,1,1
	PuPlayer.LabelNew pBackglass,"high4name",typefont,			5,16777215  ,0,1,1,22,54,1,1
	PuPlayer.LabelNew pBackglass,"high4score",numberfont,		5,16777215  ,0,1,1,36,54,1,1
	PuPlayer.LabelNew pBackglass,"high5name",typefont,			5,16777215  ,0,1,1,22,62,1,1
	PuPlayer.LabelNew pBackglass,"high5score",numberfont,		5,16777215  ,0,1,1,36,62,1,1
	PuPlayer.LabelNew pBackglass,"HighScore",typefont,			6,16777215	,0,0,1,20,30,1,1
	PuPlayer.LabelNew pBackglass,"HighScore2",typefont,			6,16777215	,0,0,1,20,30,1,1
	PuPlayer.LabelNew pBackglass,"HighScoreL1",numberfont,		8,16777215	,0,0,1,46,50,1,1
	PuPlayer.LabelNew pBackglass,"HighScoreL2",numberfont,		8,16777215	,0,0,1,50,50,1,1
	PuPlayer.LabelNew pBackglass,"HighScoreL3",numberfont,		8,16777215	,0,0,1,54,50,1,1
	PuPlayer.LabelNew pBackglass,"HighScoreL4",numberfont,		4,16777215	,0,0,1,20,50,1,1
	PuPlayer.LabelNew pBackglass,"CurImage","Arial",			50,391231   ,0,1,1, 0, 0,1,1  'new image type

	PuPlayer.LabelNew pBackglass,"AttractTitleL1",numberfont,	18,6655		,0,0,1,30,21,1,1
	PuPlayer.LabelNew pBackglass,"AttractNameL2",numberfont,	28,28671	,0,0,1,30,43,1,1
	PuPlayer.LabelNew pBackglass,"AttractScoreL3",numberfont,	16,28671	,0,0,1,30,65,1,1
	PuPlayer.LabelNew pBackglass,"AttractCreditsL4",numberfont,	10,6655		,0,0,1,30,91,1,1

	PuPlayer.LabelNew pBackglass,"GameOverPlayer1",numberfont,			6,28671	,0,0,1,25,16,1,1
	PuPlayer.LabelNew pBackglass,"ScoreGameOverPlayer1",numberfont,		6,28671	,0,0,1,25,24,1,1
	PuPlayer.LabelNew pBackglass,"GameOverPlayer2",numberfont,			6,28671	,0,0,1,75,16,1,1
	PuPlayer.LabelNew pBackglass,"ScoreGameOverPlayer2",numberfont,		6,28671	,0,0,1,75,24,1,1
	PuPlayer.LabelNew pBackglass,"GameOverPlayer3",numberfont,			6,28671	,0,0,1,25,60,1,1
	PuPlayer.LabelNew pBackglass,"ScoreGameOverPlayer3",numberfont,		6,28671	,0,0,1,25,68,1,1
	PuPlayer.LabelNew pBackglass,"GameOverPlayer4",numberfont,			6,28671	,0,0,1,75,60,1,1
	PuPlayer.LabelNew pBackglass,"ScoreGameOverPlayer4",numberfont,		6,28671	,0,0,1,75,68,1,1

	PuPlayer.LabelNew pBackglass,"AddScoreChest",numberfont,		6,28671	,0,0,1,14,69,1,1
	PuPlayer.LabelNew pBackglass,"TimerSuperMode",numberfont,			6,16777215	,0,0,1,5,9,1,1
	'obslabels
	'week
	PuPlayer.LabelNew pBackglass,"wh1n",typefont,				5,16777215  ,0,1,1,22,30,1,1
	PuPlayer.LabelNew pBackglass,"wh1s",numberfont,				5,16777215  ,0,1,1,36,30,1,1
	PuPlayer.LabelNew pBackglass,"wh2n",typefont,				5,16777215  ,0,1,1,22,30,1,1
	PuPlayer.LabelNew pBackglass,"wh2s",numberfont,				5,16777215  ,0,1,1,36,30,1,1
	PuPlayer.LabelNew pBackglass,"wh3n",typefont,				5,16777215  ,0,1,1,22,30,1,1
	PuPlayer.LabelNew pBackglass,"wh3s",numberfont,				5,16777215  ,0,1,1,36,30,1,1
	PuPlayer.LabelNew pBackglass,"wh4n",typefont,				5,16777215  ,0,1,1,22,30,1,1
	PuPlayer.LabelNew pBackglass,"wh4s",numberfont,				5,16777215  ,0,1,1,36,30,1,1
	PuPlayer.LabelNew pBackglass,"wh5n",typefont,				5,16777215  ,0,1,1,22,30,1,1
	PuPlayer.LabelNew pBackglass,"wh5s",numberfont,				5,16777215  ,0,1,1,36,30,1,1
	PuPlayer.LabelNew pBackglass,"wh6n",typefont,				5,16777215  ,0,1,1,22,30,1,1
	PuPlayer.LabelNew pBackglass,"wh6s",numberfont,				5,16777215  ,0,1,1,36,30,1,1
	PuPlayer.LabelNew pBackglass,"wh7n",typefont,				5,16777215  ,0,1,1,22,30,1,1
	PuPlayer.LabelNew pBackglass,"wh7s",numberfont,				5,16777215  ,0,1,1,36,30,1,1
	PuPlayer.LabelNew pBackglass,"wh8n",typefont,				5,16777215  ,0,1,1,22,30,1,1
	PuPlayer.LabelNew pBackglass,"wh8s",numberfont,				5,16777215  ,0,1,1,36,30,1,1
	PuPlayer.LabelNew pBackglass,"wh9n",typefont,				5,16777215  ,0,1,1,22,30,1,1
	PuPlayer.LabelNew pBackglass,"wh9s",numberfont,				5,16777215  ,0,1,1,36,30,1,1
	PuPlayer.LabelNew pBackglass,"wh10n",typefont,				5,16777215  ,0,1,1,22,30,1,1
	PuPlayer.LabelNew pBackglass,"wh10s",numberfont,			5,16777215  ,0,1,1,36,30,1,1
	' all-time
	PuPlayer.LabelNew pBackglass,"ah1n",typefont,				5,16777215  ,0,1,1,22,30,1,1
	PuPlayer.LabelNew pBackglass,"ah1s",numberfont,				5,16777215  ,0,1,1,36,30,1,1
	PuPlayer.LabelNew pBackglass,"ah2n",typefont,				5,16777215  ,0,1,1,22,30,1,1
	PuPlayer.LabelNew pBackglass,"ah2s",numberfont,				5,16777215  ,0,1,1,36,30,1,1
	PuPlayer.LabelNew pBackglass,"ah3n",typefont,				5,16777215  ,0,1,1,22,30,1,1
	PuPlayer.LabelNew pBackglass,"ah3s",numberfont,				5,16777215  ,0,1,1,36,30,1,1
	PuPlayer.LabelNew pBackglass,"ah4n",typefont,				5,16777215  ,0,1,1,22,30,1,1
	PuPlayer.LabelNew pBackglass,"ah4s",numberfont,				5,16777215  ,0,1,1,36,30,1,1
	PuPlayer.LabelNew pBackglass,"ah5n",typefont,				5,16777215  ,0,1,1,22,30,1,1
	PuPlayer.LabelNew pBackglass,"ah5s",numberfont,				5,16777215  ,0,1,1,36,30,1,1
	PuPlayer.LabelNew pBackglass,"ah6n",typefont,				5,16777215  ,0,1,1,22,30,1,1
	PuPlayer.LabelNew pBackglass,"ah6s",numberfont,				5,16777215  ,0,1,1,36,30,1,1
	PuPlayer.LabelNew pBackglass,"ah7n",typefont,				5,16777215  ,0,1,1,22,30,1,1
	PuPlayer.LabelNew pBackglass,"ah7s",numberfont,				5,16777215  ,0,1,1,36,30,1,1
	PuPlayer.LabelNew pBackglass,"ah8n",typefont,				5,16777215  ,0,1,1,22,30,1,1
	PuPlayer.LabelNew pBackglass,"ah8s",numberfont,				5,16777215  ,0,1,1,36,30,1,1
	PuPlayer.LabelNew pBackglass,"ah9n",typefont,				5,16777215  ,0,1,1,22,30,1,1
	PuPlayer.LabelNew pBackglass,"ah9s",numberfont,				5,16777215  ,0,1,1,36,30,1,1
	PuPlayer.LabelNew pBackglass,"ah10n",typefont,				5,16777215  ,0,1,1,22,30,1,1
	PuPlayer.LabelNew pBackglass,"ah10s",numberfont,			5,16777215  ,0,1,1,36,30,1,1

	'called on table load
	Sub resetbackglass
	PuPlayer.LabelSet pBackglass,"titleimg","",1,"{'mt':2,'color':111111, 'width': 0, 'height': 0, 'yalign': 0}"
	PuPlayer.LabelShowPage pBackglass,1,0,""
	'PuPlayer.playlistplayex pBackglass,"backglass","base.mp4",0,1  'should be an attract background (no text is displayed)
	'PuPlayer.SetBackground pBackglass,1	
	'top
'	PuPlayer.LabelSet pBackglass,"potionnum",0,1,"{'mt':2,'color':15066597, 'size': 4, 'xpos': 6.5, 'xalign': 1, 'ypos': 5.4, 'yalign': 1}"
'	PuPlayer.LabelSet pBackglass,"egghits",0 & "/" & 5,1,"{'mt':2,'color':2477823, 'size': 3, 'xpos': 35.8, 'xalign': 1, 'ypos': 6.2, 'yalign': 1}"
'	PuPlayer.LabelSet pBackglass,"egglocks",0,1,"{'mt':2,'color':2477823, 'size': 3, 'xpos': 35.8, 'xalign': 1, 'ypos': 11.9, 'yalign': 1}"
'	PuPlayer.LabelSet pBackglass,"lakespins",75,1,"{'mt':2,'color':2477823, 'size': 3, 'xpos': 50, 'xalign': 1, 'ypos': 6.2, 'yalign': 1}"
'	PuPlayer.LabelSet pBackglass,"lakejack",mermultivalue / 1000000 & "m",1,"{'mt':2,'color':2477823, 'size': 3, 'xpos': 50, 'xalign': 1, 'ypos': 11.9, 'yalign': 1}"
'	PuPlayer.LabelSet pBackglass,"mazeramp",0 & "/" & 8,1,"{'mt':2,'color':2477823, 'size': 3, 'xpos': 64.2, 'xalign': 1, 'ypos': 6.2, 'yalign': 1}"
'	PuPlayer.LabelSet pBackglass,"mazejacks",0,1,"{'mt':2,'color':2477823, 'size': 3, 'xpos': 64.2, 'xalign': 1, 'ypos': 11.9, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"spellnum","",1,"{'mt':2,'color':15066597, 'size': 4, 'xpos': 93.6, 'xalign': 1, 'ypos': 5.4, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"spellnum2","",1,"{'mt':2,'color':15066597, 'size': 10, 'xpos': 50, 'xalign': 1, 'ypos': 42, 'yalign': 1}"
	'bottom
'	PuPlayer.LabelSet pBackglass,"gobletnum",0,1,"{'mt':2,'color':2477823, 'size': 4, 'xpos': 8.8, 'xalign': 1, 'ypos': 88.8, 'yalign': 1}"
'	PuPlayer.LabelSet pBackglass,"deatheaternum",0,1,"{'mt':2,'color':2477823, 'size': 4, 'xpos': 17.1, 'xalign': 1, 'ypos': 88.8, 'yalign': 1}"
'	PuPlayer.LabelSet pBackglass,"modenum",0,1,"{'mt':2,'color':2477823, 'size': 4, 'xpos': 25.8, 'xalign': 1, 'ypos': 88.8, 'yalign': 1}"
'	PuPlayer.LabelSet pBackglass,"combonum",0,1,"{'mt':2,'color':15066597, 'size': 4, 'xpos': 38.9, 'xalign': 1, 'ypos': 74.8, 'yalign': 1}"
'	PuPlayer.LabelSet pBackglass,"modetimernum",0,1,"{'mt':2,'color':15066597, 'size': 4, 'xpos': 61.4, 'xalign': 1, 'ypos': 74.8, 'yalign': 1}"

'	PuPlayer.LabelSet pBackglass,"curplayer","Player 1",1,"{'mt':2,'color':2477823, 'size': 2, 'xpos': 45, 'xalign': 1, 'ypos': 83.2, 'yalign': 0}"
'	PuPlayer.LabelSet pBackglass,"Ball","Ball 0",1,"{'mt':2,'color':2477823, 'size': 2, 'xpos': 50, 'xalign': 0, 'ypos': 83.2, 'yalign': 0}"
'	PuPlayer.LabelSet pBackglass,"curscore","x",1,"{'mt':2,'color':2477823, 'size': 6.2, 'xpos': 50, 'xalign': 1, 'ypos': 89.2, 'yalign': 1}"


	PuPlayer.LabelSet pBackglass,"curplayer","",1,"{'mt':2,'color':2477823, 'size': 3, 'xpos': 7.5, 'xalign': 1, 'ypos': 94.5, 'yalign': 0}"
	PuPlayer.LabelSet pBackglass,"Ball","",1,"{'mt':2,'color':2477823, 'size': 3, 'xpos': 87.5, 'xalign': 0, 'ypos': 94.5, 'yalign': 0}"
	PuPlayer.LabelSet pBackglass,"Ball2","",1,"{'mt':2,'color':2477823, 'size': 4, 'xpos': 18, 'xalign': 0, 'ypos': 89.2, 'yalign': 0}"
	PuPlayer.LabelSet pBackglass,"curscore","",1,"{'mt':2,'color':2477823, 'size': 6.2, 'xpos': 50, 'xalign': 1, 'ypos': 89.2, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"curscore2","",1,"{'mt':2,'color':2477823, 'size': 10, 'xpos': 50, 'xalign': 1, 'ypos': 20, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"curscore3","",1,"{'mt':2,'color':2477823, 'size': 6.2, 'xpos': 50, 'xalign': 1, 'ypos': 89.2, 'yalign': 1}"
	
	PuPlayer.LabelSet pBackglass,"TitleBonus","",1,"{'mt':2,'color':814827, 'size': 15, 'xpos': 50, 'xalign': 1, 'ypos': 18, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"TextBaseBonus","",1,"{'mt':2,'color':814827, 'size': 8, 'xpos': 47, 'xalign': 2, 'ypos': 34.4, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"TextSuperFeatures","",1,"{'mt':2,'color':814827, 'size': 8, 'xpos': 47, 'xalign': 2, 'ypos': 49.2, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"TextCombos","",1,"{'mt':2,'color':814827, 'size': 8, 'xpos': 47, 'xalign': 2, 'ypos': 62.9, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"TextLoops","",1,"{'mt':2,'color':814827, 'size': 8, 'xpos': 47, 'xalign': 2, 'ypos': 77.5, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"TextTotalBonus","",1,"{'mt':2,'color':814827, 'size': 20, 'xpos': 50, 'xalign': 1, 'ypos': 37, 'yalign': 1}"

	PuPlayer.LabelSet pBackglass,"BaseBonus","",1,"{'mt':2,'color':814827, 'size': 6, 'xpos': 55, 'xalign': 0, 'ypos': 34.4, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"SuperFeatures","",1,"{'mt':2,'color':814827, 'size': 6, 'xpos': 55, 'xalign': 0, 'ypos': 47.2, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"Combos","",1,"{'mt':2,'color':814827, 'size': 6, 'xpos': 55, 'xalign': 0, 'ypos': 62.9, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"Loops","",1,"{'mt':2,'color':814827, 'size': 6, 'xpos': 55, 'xalign': 0, 'ypos': 77.5, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"BonusMultiplier","",1,"{'mt':2,'color':814827, 'size': 40, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"TotalBonus","",1,"{'mt':2,'color':814827, 'size': 16, 'xpos': 50, 'xalign': 1, 'ypos': 57, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"TotalScoreBonus","",1,"{'mt':2,'color':814827, 'size': 12, 'xpos': 50, 'xalign': 1, 'ypos': 89, 'yalign': 1}"
	
	PuPlayer.LabelSet pBackglass,"AddScoreDisplayed","",1,"{'mt':2,'color':255, 'size': 12, 'xpos': 50, 'xalign': 1, 'ypos': 50, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"AddScoreDisplayed2","",1,"{'mt':2,'color':0, 'size': 12, 'xpos': 50, 'xalign': 1, 'ypos': 50, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"CommentDisplayed","",1,"{'mt':2,'color':255, 'size': 10, 'xpos': 50, 'xalign': 1, 'ypos': 35, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"CommentDisplayed2","",0,"{'mt':1,'color':2477823, 'size': 10, 'xpos': 50, 'xalign': 1, 'ypos': 35, 'yalign': 1, 'at':3,'hstart':50,'hend':180,'len':" & (2000) & ",'mlen':" & (1000) & ",'tt':15,'fc':" & 200 & "}"

	PuPlayer.LabelSet pBackglass,"Mystery","",1,"{'mt':2,'color':30719, 'size': 12, 'xpos': 50, 'xalign': 1, 'ypos': 50, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"RandomMystery","",1,"{'mt':2,'color':30719, 'size': 10, 'xpos': 50, 'xalign': 1, 'ypos': 35, 'yalign': 1}"

	PuPlayer.LabelSet pBackglass,"LastChanceTime ","",0,"{'mt':1,'color':255, 'size': 20, 'xpos': 50, 'xalign': 1, 'ypos': 50, 'yalign': 1}"
	
'	PuPlayer.LabelSet pBackglass,"ruletitle","Black Knight Sword of Rage",1,"{'mt':2,'color':15066597, 'size': 2, 'xpos': 82.5, 'xalign': 1, 'ypos': 86.8, 'yalign': 1}"
'	PuPlayer.LabelSet pBackglass,"rulecopy1","Games Played: " & TotalGamesPlayed,1,"{'mt':2,'color':15066597, 'size': 2, 'xpos': 82.5, 'xalign': 1, 'ypos': 89.8, 'yalign': 1}"
'	PuPlayer.LabelSet pBackglass,"rulecopy2","This is a fan game meant only for fun.",1,"{'mt':2,'color':15066597, 'size': 2, 'xpos': 82.5, 'xalign': 1, 'ypos': 92.8, 'yalign': 1}"




	End Sub

	'called each time a player changes
	Sub currentplayerbackglass
'		puPlayer.LabelSet pBackglass,"titleimg","",1,"{'mt':2,'color':111111, 'width': 0, 'height': 0, 'yalign': 0}"
'		PuPlayer.LabelSet pBackglass,"potionnum",potneeded - potions(CurrentPlayer),1,"{'mt':2,'color':15066597, 'size': 4, 'xpos': 6.5, 'xalign': 1, 'ypos': 5.4, 'yalign': 1}"
'		PuPlayer.LabelSet pBackglass,"egglocks",dragonlock(CurrentPlayer),1,"{'mt':2,'color':2697513, 'size': 3, 'xpos': 35.8, 'xalign': 1, 'ypos': 11.9, 'yalign': 1}"
'		PuPlayer.LabelSet pBackglass,"mazejacks",mazejacks(CurrentPlayer),1,"{'mt':2,'color':2697513, 'size': 3, 'xpos': 64.2, 'xalign': 1, 'ypos': 11.9, 'yalign': 1}"
		
		If goblets(CurrentPlayer) > 15 Then
'		PuPlayer.LabelSet pBackglass,"gobletnum",0,1,"{'mt':2,'color':2697513, 'size': 4, 'xpos': 8.8, 'xalign': 1, 'ypos': 88.8, 'yalign': 1}"
		Else
'		PuPlayer.LabelSet pBackglass,"gobletnum",15 - goblets(CurrentPlayer),1,"{'mt':2,'color':2697513, 'size': 4, 'xpos': 8.8, 'xalign': 1, 'ypos': 88.8, 'yalign': 1}"
		end If

		If totalbumps(CurrentPlayer) < 75 Then
'			PuPlayer.LabelSet pBackglass,"deatheaternum",75 - totalbumps(CurrentPlayer),1,"{'mt':2,'color':2697513, 'size': 4, 'xpos': 17.1, 'xalign': 1, 'ypos': 88.8, 'yalign': 1}"
		Else
'			PuPlayer.LabelSet pBackglass,"deatheaternum",0,1,"{'mt':2,'color':2697513, 'size': 4, 'xpos': 17.1, 'xalign': 1, 'ypos': 88.8, 'yalign': 1}"
			checkmerlinsecond
		end if

'		PuPlayer.LabelSet pBackglass,"combonum",10 - totalcombo(CurrentPlayer),1,"{'mt':2,'color':15066597, 'size': 4, 'xpos': 38.9, 'xalign': 1, 'ypos': 74.8, 'yalign': 1}"

		if modescompleted(CurrentPlayer) > 3 Then
'			PuPlayer.LabelSet pBackglass,"modenum",0,1,"{'mt':2,'color':2697513, 'size': 4, 'xpos': 25.8, 'xalign': 1, 'ypos': 88.8, 'yalign': 1}"
		Else
'			PuPlayer.LabelSet pBackglass,"modenum",3-modescompleted(CurrentPlayer),1,"{'mt':2,'color':2697513, 'size': 4, 'xpos': 25.8, 'xalign': 1, 'ypos': 88.8, 'yalign': 1}"
		end if
'		setpotionname
	End Sub

	titlepos = 0
	titletimer.enabled = 0
	title = ""
	
	subtitle = ""

	Sub titletimer_timer
		titlepos = titlepos + 1
		Select Case titlepos
			Case 1
				PuPlayer.LabelSet pBackglass,"titleimg","DMDImages\\popframe.png",1,"{'mt':2,'width': 99, 'height': 99, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':2565927, 'size': 0, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 0, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':2565927, 'size': 0, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 0, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"

			Case 2
				PuPlayer.LabelSet pBackglass,"textbg","",1,""
				PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':5066061, 'size': 0.6, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 0.6, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':5066061, 'size': 0.4, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 0.4, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			Case 3
				PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':7960953, 'size': 1.2, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 1.2, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':7960953, 'size': 0.8, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 0.8, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			Case 4
				PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':9671571, 'size': 1.8, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 1.8, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':9671571, 'size': 1.2, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 1.2, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			Case 5
				PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':11842740, 'size': 2.4, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 2.4, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':11842740, 'size': 1.6, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 1.6, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			Case 6
				PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':13224393, 'size': 3, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 3, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':13224393, 'size': 2, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 2, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			Case 7
				PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':14671839, 'size': 3.6, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 3.6, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':14671839, 'size': 2.4, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 2.4, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			Case 8
				PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':15790320, 'size': 4.2, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 4.2, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':15790320, 'size': 2.8, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 2.8, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			Case 9
				PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':16316664, 'size': 4.8, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 4.8, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':16316664, 'size': 3.2, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 3.2, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			Case 10
				PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':16777215, 'size': 5.4, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 5.4, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':16777215, 'size': 3.6, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 3.6, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			Case 11
				puPlayer.LabelSet pBackglass,"titleimg","DMDImages\\popframe.png",1,"{'mt':2,'width': 90, 'height': 90, 'yalign': 1}"
				PuPlayer.LabelSet pBackglass,"title",title,1,"{'mt':2,'color':16777215, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 36.4, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg",title,1,"{'mt':2,'color':0, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"title2",subtitle,1,"{'mt':2,'color':16777215, 'size': 4, 'xpos': 50, 'xalign': 1, 'ypos': 45.3, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg2",subtitle,1,"{'mt':2,'color':0, 'size': 4, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"

			Case 150
				puPlayer.LabelSet pBackglass,"titleimg","",1,"{'mt':2,'color':111111, 'width': 0, 'height': 0, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"title","",1,"{'mt':2,'color':16777215, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg","",1,"{'mt':2,'color':0, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"title2","",1,"{'mt':2,'color':16777215, 'size': 4, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"titlebg2","",1,"{'mt':2,'color':0, 'size': 4, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
				titlepos = 0
				titletimer.enabled = 0
		End Select
	End Sub

	
	Sub pNote(msgText,msg2text)
		If usePuPDMD=true then exit sub
		title = msgText
		subtitle = msg2text
		If titlepos = 0 Then
			titletimer.enabled = 1
		Else
			titlepos = 0
			titletimer.enabled = 1
			PuPlayer.LabelSet pBackglass,"title","",1,"{'mt':2,'color':16777215, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"titlebg","",1,"{'mt':2,'color':0, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"title2","",1,"{'mt':2,'color':16777215, 'size': 4, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
			PuPlayer.LabelSet pBackglass,"titlebg2","",1,"{'mt':2,'color':0, 'size': 4, 'xpos': 50, 'xalign': 1, 'ypos': 45, 'yalign': 0}"
		End If
	End Sub

	sub scoretimer_timer
		pUpdateScores
	end Sub

Sub MatchingScore
	If MatchRandom = 0 Then
		PuPlayer.LabelSet pBackglass,"matchscore","00",1,"{'mt':2,'color':0, 'size': 35 }"
	Else
		PuPlayer.LabelSet pBackglass,"matchscore","" & FormatNumber(MatchRandom,0),1,"{'mt':2,'color':0, 'size': 35 }"
	End If
End Sub
'*******************************************************************************************************
'       PuPlayer.LabelSet pBackglass,"spellnum",0,1,"{'mt':2,'color':15066597, 'size': 4, 'xpos': 93.6, 'xalign': 1, 'ypos': 5.4, 'yalign': 1}"
'*------------------------------------------------------------------------------------
'*------------------------------ Score Update Here -----------------------------------
'*------------------------------------------------------------------------------------


Sub pUpdateScores
	dim ballnum
	ballnum = bpgcurrent - BallsRemaining(CurrentPlayer) + 1
	
	If HideOverlay = True Then
		PuPlayer.LabelSet pBackglass,"curscore","",1,""
		PuPlayer.LabelSet pBackglass,"curscore2","",1,""
		PuPlayer.LabelSet pBackglass,"curscore3","",1,""
		PuPlayer.LabelSet pBackglass,"curplayer","",1,""
		PuPlayer.LabelSet pBackglass,"spellnum2","",1,"{'mt':2,'color':3753717, 'size': 4 }"
		PuPlayer.LabelSet pBackglass,"matchscore","",1,""
		PuPlayer.LabelSet pBackglass,"Play1score","",1,"{'mt':2,'color':3753717, 'size': 4 }"
		PuPlayer.LabelSet pBackglass,"Play1","Player 1",1,"{'mt':2,'color':2477823}"
		PuPlayer.LabelSet pBackglass,"Play2score","",1,"{'mt':2,'color':3753717, 'size': 4 }"
		PuPlayer.LabelSet pBackglass,"Play2","Player 2",1,"{'mt':2,'color':2477823}"
		PuPlayer.LabelSet pBackglass,"Play3score","",1,"{'mt':2,'color':3753717, 'size': 4 }"
		PuPlayer.LabelSet pBackglass,"Play3","Player 3",1,"{'mt':2,'color':2477823}"
		PuPlayer.LabelSet pBackglass,"Play4score","",1,"{'mt':2,'color':3753717, 'size': 4 }"
		PuPlayer.LabelSet pBackglass,"Play4","Player 4",1,"{'mt':2,'color':2477823}"
		PuPlayer.LabelSet pBackglass,"Ball","",1,""
		puPlayer.LabelSet pBackglass,"Ball2","",1,""

	Else 	
		If Gamerover_flag = True And bAttractMode = False Then
			If PlayersPlayingGame = 1 Then
				PuPlayer.LabelSet pBackglass,"curscore",FormatNumber(Score(CurrentPlayer),0),1,""
				PuPlayer.LabelSet pBackglass,"curplayer","Credits " & Credits,1,""
				PuPlayer.LabelSet pBackglass,"Ball","",1,"{'mt':2,'color':2477823, 'size': 3, 'xpos': 87.5, 'xalign': 0, 'ypos': 83.2, 'yalign': 0}"
'				PuPlayer.LabelSet pBackglass,"Play1score","" & FormatNumber(Score(1),0),1,"{'mt':2,'color':3753717, 'size': 4 }"							
'				PuPlayer.LabelSet pBackglass,"Play1","Player 1",1,"{'mt':2,'color':2477823}"
			End If
			If PlayersPlayingGame = 2 Then
				PuPlayer.LabelSet pBackglass,"Play1score","" & FormatNumber(Score(1),0),1,"{'mt':2,'color':3753717, 'size': 4 }"
				PuPlayer.LabelSet pBackglass,"Play1","Player 1",1,"{'mt':2,'color':2477823}"
				PuPlayer.LabelSet pBackglass,"Play2score","" & FormatNumber(Score(2),0),1,"{'mt':2,'color':3753717, 'size': 4 }"
				PuPlayer.LabelSet pBackglass,"Play2","Player 2",1,"{'mt':2,'color':3753717}"
				PuPlayer.LabelSet pBackglass,"Ball","",1,"{'mt':2,'color':2477823, 'size': 3, 'xpos': 87.5, 'xalign': 0, 'ypos': 83.2, 'yalign': 0}"
			End If
			If PlayersPlayingGame = 3 Then
				PuPlayer.LabelSet pBackglass,"Play1score","" & FormatNumber(Score(1),0),1,"{'mt':2,'color':3753717, 'size': 4 }"
				PuPlayer.LabelSet pBackglass,"Play1","Player 1",1,"{'mt':2,'color':2477823}"
				PuPlayer.LabelSet pBackglass,"Play2score","" & FormatNumber(Score(2),0),1,"{'mt':2,'color':3753717, 'size': 4 }"
				PuPlayer.LabelSet pBackglass,"Play2","Player 2",1,"{'mt':2,'color':3753717}"
				PuPlayer.LabelSet pBackglass,"Play3score","" & FormatNumber(Score(3),0),1,"{'mt':2,'color':3753717, 'size': 4 }"
				PuPlayer.LabelSet pBackglass,"Play3","Player 3",1,"{'mt':2,'color':3753717}"
				PuPlayer.LabelSet pBackglass,"Ball","",1,"{'mt':2,'color':2477823, 'size': 3, 'xpos': 87.5, 'xalign': 0, 'ypos': 83.2, 'yalign': 0}"
			End If
			If PlayersPlayingGame = 4 Then
				PuPlayer.LabelSet pBackglass,"Play1score","" & FormatNumber(Score(1),0),1,"{'mt':2,'color':3753717, 'size': 4 }"
				PuPlayer.LabelSet pBackglass,"Play1","Player 1",1,"{'mt':2,'color':2477823}"
				PuPlayer.LabelSet pBackglass,"Play2score","" & FormatNumber(Score(2),0),1,"{'mt':2,'color':3753717, 'size': 4 }"
				PuPlayer.LabelSet pBackglass,"Play2","Player 2",1,"{'mt':2,'color':3753717}"
				PuPlayer.LabelSet pBackglass,"Play3score","" & FormatNumber(Score(3),0),1,"{'mt':2,'color':3753717, 'size': 4 }"
				PuPlayer.LabelSet pBackglass,"Play3","Player 3",1,"{'mt':2,'color':3753717}"
				PuPlayer.LabelSet pBackglass,"Play4score","" & FormatNumber(Score(4),0),1,"{'mt':2,'color':3753717, 'size': 4 }"
				PuPlayer.LabelSet pBackglass,"Play4","Player 4",1,"{'mt':2,'color':3753717}"
				PuPlayer.LabelSet pBackglass,"Ball","",1,"{'mt':2,'color':2477823, 'size': 3, 'xpos': 87.5, 'xalign': 0, 'ypos': 83.2, 'yalign': 0}"
			End If

		ElseIf NewRecord = True Then
			PuPlayer.LabelSet pBackglass,"curscore","",1,""
			PuPlayer.LabelSet pBackglass,"curscore2","",1,""
			PuPlayer.LabelSet pBackglass,"curscore3","",1,""
			PuPlayer.LabelSet pBackglass,"curplayer","",1,""
			PuPlayer.LabelSet pBackglass,"spellnum2","",1,"{'mt':2,'color':3753717, 'size': 4 }"
			PuPlayer.LabelSet pBackglass,"matchscore","",1,""
			PuPlayer.LabelSet pBackglass,"Play1score","",1,"{'mt':2,'color':3753717, 'size': 4, 'xpos': 12.5, 'xalign': 1, 'ypos': 94.5, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"Play2score","",1,"{'mt':2,'color':3753717, 'size': 4, 'xpos': 37.5, 'xalign': 1, 'ypos': 94.5, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"Play3score","",1,"{'mt':2,'color':3753717, 'size': 4, 'xpos': 62.5, 'xalign': 1, 'ypos': 94.5, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"Play4score","",1,"{'mt':2,'color':3753717, 'size': 4, 'xpos': 87.5, 'xalign': 1, 'ypos': 94.5, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"Ball","",1,"{'mt':2,'color':2477823, 'size': 3, 'xpos': 87.5, 'xalign': 0, 'ypos': 83.2, 'yalign': 0}"

		ElseIf bAttractMode = True Then
			PuPlayer.LabelSet pBackglass,"curscore","",1,""
			PuPlayer.LabelSet pBackglass,"curplayer","",1,""
			PuPlayer.LabelSet pBackglass,"Play1score","",1,"{'mt':2,'color':3753717, 'size': 4 }"
			PuPlayer.LabelSet pBackglass,"Play1","",1,"{'mt':2,'color':2477823}"
			PuPlayer.LabelSet pBackglass,"Play2score","",1,"{'mt':2,'color':3753717, 'size': 4 }"
			PuPlayer.LabelSet pBackglass,"Play2","",1,"{'mt':2,'color':3753717}"
			PuPlayer.LabelSet pBackglass,"Play3score","",1,"{'mt':2,'color':3753717, 'size': 4 }"
			PuPlayer.LabelSet pBackglass,"Play3","",1,"{'mt':2,'color':3753717}"
			PuPlayer.LabelSet pBackglass,"Play4score","",1,"{'mt':2,'color':3753717, 'size': 4 }"
			PuPlayer.LabelSet pBackglass,"Play4","",1,"{'mt':2,'color':3753717}"
			PuPlayer.LabelSet pBackglass,"Ball","",1,"{'mt':2,'color':2477823, 'size': 3, 'xpos': 87.5, 'xalign': 0, 'ypos': 83.2, 'yalign': 0}"


'*************************************************************************************************************************************************************************************************************************
	

			
		ElseIf CurrentMissionFlag(CurrentPlayer) = 2 Then
			If BlackKnightRetro(CurrentPlayer) = 1 Then
				PuPlayer.LabelSet pBackglass,"curscore3",FormatNumber(Score(CurrentPlayer),0) & " ",1,""
				PuPlayer.LabelSet pBackglass,"Ball2",""  &  BallsOnPlayfield & "  ",1,""
				puPlayer.LabelSet pBackglass,"Ball","",1,""
			ElseIf BlackKnightRetro(CurrentPlayer) = 3 Then
				PuPlayer.LabelSet pBackglass,"curscore2",FormatNumber(Score(CurrentPlayer),0),1,""
				If AddTimeForMission(CurrentPlayer) = 0 Then 
					PuPlayer.LabelSet pBackglass,"spellnum2","",1,"{'mt':2,'color':2477823, 'size': 10 }"
				Else
					PuPlayer.LabelSet pBackglass,"spellnum2","" & FormatNumber(AddTimeForMission(CurrentPlayer),0) & " SECONDS",1,"{'mt':2,'color':2477823, 'size': 10 }"
				End If		
			End If
			PuPlayer.LabelSet pBackglass,"curscore","",1,""
			PuPlayer.LabelSet pBackglass,"curplayer","",1,""
			PuPlayer.LabelSet pBackglass,"curscore2","",1,""
			PuPlayer.LabelSet pBackglass,"curscore3","",1,""
		
'*************************************************************************************************************************************************************************************************************************
'******************************************************************************** OVERLAY DISPLAYED DURING GAME NO MODE STARTED ******************************************************************************************
'*************************************************************************************************************************************************************************************************************************
		ElseIf CurrentMissionFlag(CurrentPlayer) = 0 or 1 Then
			If CurrentMissionFlag(CurrentPlayer) = 1 And AddTimeForMission(CurrentPlayer) > 0 Then
				PuPlayer.LabelSet pBackglass,"spellnum","" & FormatNumber(AddTimeForMission(CurrentPlayer),0),1,"{'mt':2,'color':15066597, 'size': 4 }"
			Else
				PuPlayer.LabelSet pBackglass,"spellnum","",1,"{'mt':2,'color':15066597, 'size': 4 }"
			End If
			
			PuPlayer.LabelSet pBackglass,"curscore2","",1,""					' Hide - used For BK RETRO Mode
			PuPlayer.LabelSet pBackglass,"curscore3","",1,""					' Hide - used For BK RETRO Mode
			puPlayer.LabelSet pBackglass,"Ball2","",1,""						' Hide - used For BK RETRO Mode

			if ballnum > bpgcurrent Then
				PuPlayer.LabelSet pBackglass,"Ball","",1,""
				puPlayer.LabelSet pBackglass,"Ball2","",1,""
			end If
			'******************************************* 1 PLAYER ****************************************
			If PlayersPlayingGame = 1 Then
				If CurrentPlayer = 1 Then
					PuPlayer.LabelSet pBackglass,"Play1score","",1,"{'mt':2,'color':4491006, 'size': 4, 'xpos': 100, 'xalign': 1, 'ypos': 100, 'yalign': 1 }"
					PuPlayer.LabelSet pBackglass,"Play1","Player 1",1,"{'mt':2,'color':3753717 }"
					PuPlayer.LabelSet pBackglass,"curscore",FormatNumber(Score(CurrentPlayer),0),1,""
					PuPlayer.LabelSet pBackglass,"curplayer","Credits " & Credits,1,""
					PuPlayer.LabelSet pBackglass,"Ball","Ball "  &  bpgcurrent - BallsRemaining(CurrentPlayer) + 1 ,1,""
				End If
			'******************************************* 2 PLAYER ****************************************			
			ElseIf PlayersPlayingGame = 2 Then
				PuPlayer.LabelSet pBackglass,"curscore",FormatNumber(Score(CurrentPlayer),0),1,"{'mt':2,'color':2477823, 'size': 6.2, 'xpos': 50, 'xalign': 1, 'ypos': 82.2, 'yalign': 1}"
				PuPlayer.LabelSet pBackglass,"curplayer","Credits " & Credits,1,"{'mt':2,'color':2477823, 'size': 3, 'xpos': 7.5, 'xalign': 1, 'ypos': 83.2, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"Ball","Ball "  &  bpgcurrent - BallsRemaining(CurrentPlayer) + 1 ,1,"{'mt':2,'color':2477823, 'size': 3, 'xpos': 87.5, 'xalign': 0, 'ypos': 83.2, 'yalign': 0}"
				If CurrentPlayer = 1 Then
					PuPlayer.LabelSet pBackglass,"Play1score","PLAYER 1",1,"{'mt':2,'color':4491006, 'size': 4, 'xpos': 12.5, 'xalign': 1, 'ypos': 94.5, 'yalign': 1 }"
					PuPlayer.LabelSet pBackglass,"Play1","Player 1",1,"{'mt':2,'color':3753717 }"
					PuPlayer.LabelSet pBackglass,"Play2score","" & FormatNumber(Score(2),0),1,"{'mt':2,'color':3753717, 'size': 4, 'xpos': 87.5, 'xalign': 1, 'ypos': 94.5, 'yalign': 1 }"
					PuPlayer.LabelSet pBackglass,"Play2","Player 2",1,"{'mt':2,'color':3753717}"
				ElseIf CurrentPlayer = 2 Then
					PuPlayer.LabelSet pBackglass,"Play1score","" & FormatNumber(Score(1),0),1,"{'mt':2,'color':3753717, 'size': 4, 'xpos': 12.5, 'xalign': 1, 'ypos': 94.5, 'yalign': 1 }"
					PuPlayer.LabelSet pBackglass,"Play1","Player 1",1,"{'mt':2,'color':2477823}"
					PuPlayer.LabelSet pBackglass,"Play2score","PLAYER 2",1,"{'mt':2,'color':4491006, 'size': 4, 'xpos': 87.5, 'xalign': 1, 'ypos': 94.5, 'yalign': 1 }"
					PuPlayer.LabelSet pBackglass,"Play2","Player 2",1,"{'mt':2,'color':2477823 }"
				End If
			'******************************************* 3 PLAYER ****************************************			
			ElseIf PlayersPlayingGame = 3 Then
				PuPlayer.LabelSet pBackglass,"curscore",FormatNumber(Score(CurrentPlayer),0),1,"{'mt':2,'color':2477823, 'size': 6.2, 'xpos': 50, 'xalign': 1, 'ypos': 77.2, 'yalign': 1}"
				PuPlayer.LabelSet pBackglass,"curplayer","Credits " & Credits,1,"{'mt':2,'color':2477823, 'size': 3, 'xpos': 7.5, 'xalign': 1, 'ypos': 83.2, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"Ball","Ball "  &  bpgcurrent - BallsRemaining(CurrentPlayer) + 1 ,1,"{'mt':2,'color':2477823, 'size': 3, 'xpos': 87.5, 'xalign': 0, 'ypos': 83.2, 'yalign': 0}"
				If CurrentPlayer = 1 Then
					PuPlayer.LabelSet pBackglass,"Play1score","PLAYER 1",1,"{'mt':2,'color':4491006, 'size': 4, 'xpos': 12.5, 'xalign': 1, 'ypos': 94.5, 'yalign': 1 }"
					PuPlayer.LabelSet pBackglass,"Play1","Player 1",1,"{'mt':2,'color':3753717 }"
					PuPlayer.LabelSet pBackglass,"Play2score","" & FormatNumber(Score(2),0),1,"{'mt':2,'color':3753717, 'size': 4, 'xpos': 37.5, 'xalign': 1, 'ypos': 94.5, 'yalign': 1 }"
					PuPlayer.LabelSet pBackglass,"Play2","Player 2",1,"{'mt':2,'color':3753717}"
					PuPlayer.LabelSet pBackglass,"Play3score","" & FormatNumber(Score(3),0),1,"{'mt':2,'color':3753717, 'size': 4, 'xpos': 62.5, 'xalign': 1, 'ypos': 94.5, 'yalign': 1 }"
					PuPlayer.LabelSet pBackglass,"Play3","Player 3",1,"{'mt':2,'color':3753717}"
				ElseIf CurrentPlayer = 2 Then
					PuPlayer.LabelSet pBackglass,"Play1score","" & FormatNumber(Score(1),0),1,"{'mt':2,'color':3753717, 'size': 4, 'xpos': 12.5, 'xalign': 1, 'ypos': 94.5, 'yalign': 1 }"
					PuPlayer.LabelSet pBackglass,"Play1","Player 1",1,"{'mt':2,'color':2477823}"
					PuPlayer.LabelSet pBackglass,"Play2score","PLAYER 2",1,"{'mt':2,'color':4491006, 'size': 4, 'xpos': 37.5, 'xalign': 1, 'ypos': 94.5, 'yalign': 1 }"
					PuPlayer.LabelSet pBackglass,"Play2","Player 2",1,"{'mt':2,'color':2477823 }"
					PuPlayer.LabelSet pBackglass,"Play3score","" & FormatNumber(Score(3),0),1,"{'mt':2,'color':3753717, 'size': 4, 'xpos': 62.5, 'xalign': 1, 'ypos': 94.5, 'yalign': 1 }"
					PuPlayer.LabelSet pBackglass,"Play3","Player 3",1,"{'mt':2,'color':2477823}"
				ElseIf CurrentPlayer = 3 Then
					PuPlayer.LabelSet pBackglass,"Play1score","" & FormatNumber(Score(1),0),1,"{'mt':2,'color':3753717, 'size': 4, 'xpos': 12.5, 'xalign': 1, 'ypos': 94.5, 'yalign': 1 }"
					PuPlayer.LabelSet pBackglass,"Play1","Player 1",1,"{'mt':2,'color':2477823}"
					PuPlayer.LabelSet pBackglass,"Play2score","" & FormatNumber(Score(2),0),1,"{'mt':2,'color':3753717, 'size': 4, 'xpos': 37.5, 'xalign': 1, 'ypos': 94.5, 'yalign': 1 }"
					PuPlayer.LabelSet pBackglass,"Play2","Player 2",1,"{'mt':2,'color':2477823}"
					PuPlayer.LabelSet pBackglass,"Play3score","PLAYER 3",1,"{'mt':2,'color':4491006, 'size': 4, 'xpos': 62.5, 'xalign': 1, 'ypos': 94.5, 'yalign': 1 }"
					PuPlayer.LabelSet pBackglass,"Play3","Player 3",1,"{'mt':2,'color':2477823 }"
				End If
'******************************************* 4 PLAYER ****************************************	
			ElseIf PlayersPlayingGame = 4 Then
				PuPlayer.LabelSet pBackglass,"curscore",FormatNumber(Score(CurrentPlayer),0),1,"{'mt':2,'color':2477823, 'size': 6.2, 'xpos': 50, 'xalign': 1, 'ypos': 77.2, 'yalign': 1}"
				PuPlayer.LabelSet pBackglass,"curplayer","Credits " & Credits,1,"{'mt':2,'color':2477823, 'size': 3, 'xpos': 7.5, 'xalign': 1, 'ypos': 83.2, 'yalign': 0}"
				PuPlayer.LabelSet pBackglass,"Ball","Ball "  &  bpgcurrent - BallsRemaining(CurrentPlayer) + 1 ,1,"{'mt':2,'color':2477823, 'size': 3, 'xpos': 87.5, 'xalign': 0, 'ypos': 83.2, 'yalign': 0}"
				If CurrentPlayer = 1 Then
					PuPlayer.LabelSet pBackglass,"Play1score","PLAYER 1",1,"{'mt':2,'color':4491006, 'size': 4, 'xpos': 12.5, 'xalign': 1, 'ypos': 94.5, 'yalign': 1}"
					PuPlayer.LabelSet pBackglass,"Play1","Player 1",1,"{'mt':2,'color':3753717 }"
					PuPlayer.LabelSet pBackglass,"Play2score","" & FormatNumber(Score(2),0),1,"{'mt':2,'color':3753717, 'size': 4, 'xpos': 37.5, 'xalign': 1, 'ypos': 94.5, 'yalign': 1 }"
					PuPlayer.LabelSet pBackglass,"Play2","Player 2",1,"{'mt':2,'color':3753717}"
					PuPlayer.LabelSet pBackglass,"Play3score","" & FormatNumber(Score(3),0),1,"{'mt':2,'color':3753717, 'size': 4, 'xpos': 62.5, 'xalign': 1, 'ypos': 94.5, 'yalign': 1 }"
					PuPlayer.LabelSet pBackglass,"Play3","Player 3",1,"{'mt':2,'color':3753717}"
					PuPlayer.LabelSet pBackglass,"Play4score","" & FormatNumber(Score(4),0),1,"{'mt':2,'color':3753717, 'size': 4, 'xpos': 87.5, 'xalign': 1, 'ypos': 94.5, 'yalign': 1 }"
					PuPlayer.LabelSet pBackglass,"Play4","Player 4",1,"{'mt':2,'color':3753717}"
				ElseIf CurrentPlayer = 2 Then
					PuPlayer.LabelSet pBackglass,"Play1score","" & FormatNumber(Score(1),0),1,"{'mt':2,'color':3753717, 'size': 4, 'xpos': 12.5, 'xalign': 1, 'ypos': 94.5, 'yalign': 1 }"
					PuPlayer.LabelSet pBackglass,"Play1","Player 1",1,"{'mt':2,'color':2477823}"
					PuPlayer.LabelSet pBackglass,"Play2score","PLAYER 2",1,"{'mt':2,'color':4491006, 'size': 4, 'xpos': 37.5, 'xalign': 1, 'ypos': 94.5, 'yalign': 1 }"
					PuPlayer.LabelSet pBackglass,"Play2","Player 2",1,"{'mt':2,'color':2477823 }"
					PuPlayer.LabelSet pBackglass,"Play3score","" & FormatNumber(Score(3),0),1,"{'mt':2,'color':3753717, 'size': 4, 'xpos': 62.5, 'xalign': 1, 'ypos': 94.5, 'yalign': 1 }"
					PuPlayer.LabelSet pBackglass,"Play3","Player 3",1,"{'mt':2,'color':2477823}"
					PuPlayer.LabelSet pBackglass,"Play4score","" & FormatNumber(Score(4),0),1,"{'mt':2,'color':3753717, 'size': 4, 'xpos': 87.5, 'xalign': 1, 'ypos': 94.5, 'yalign': 1 }"
					PuPlayer.LabelSet pBackglass,"Play4","Player 4",1,"{'mt':2,'color':2477823}"
				ElseIf CurrentPlayer = 3 Then
					PuPlayer.LabelSet pBackglass,"Play1score","" & FormatNumber(Score(1),0),1,"{'mt':2,'color':3753717, 'size': 4, 'xpos': 12.5, 'xalign': 1, 'ypos': 94.5, 'yalign': 1}"
					PuPlayer.LabelSet pBackglass,"Play1","Player 1",1,"{'mt':2,'color':2477823}"
					PuPlayer.LabelSet pBackglass,"Play2score","" & FormatNumber(Score(2),0),1,"{'mt':2,'color':3753717, 'size': 4, 'xpos': 37.5, 'xalign': 1, 'ypos': 94.5, 'yalign': 1 }"
					PuPlayer.LabelSet pBackglass,"Play2","Player 2",1,"{'mt':2,'color':2477823}"
					PuPlayer.LabelSet pBackglass,"Play3score","PLAYER 3",1,"{'mt':2,'color':4491006, 'size': 4, 'xpos': 62.5, 'xalign': 1, 'ypos': 94.5, 'yalign': 1 }"
					PuPlayer.LabelSet pBackglass,"Play3","Player 3",1,"{'mt':2,'color':2477823 }"
					PuPlayer.LabelSet pBackglass,"Play4score","" & FormatNumber(Score(4),0),1,"{'mt':2,'color':3753717, 'size': 4, 'xpos': 87.5, 'xalign': 1, 'ypos': 94.5, 'yalign': 1 }"
					PuPlayer.LabelSet pBackglass,"Play4","Player 4",1,"{'mt':2,'color':2477823}"
				ElseIf CurrentPlayer = 4 Then
					PuPlayer.LabelSet pBackglass,"Play1score","" & FormatNumber(Score(1),0),1,"{'mt':2,'color':3753717, 'size': 4, 'xpos': 12.5, 'xalign': 1, 'ypos': 94.5, 'yalign': 1 }"
					PuPlayer.LabelSet pBackglass,"Play1","Player 1",1,"{'mt':2,'color':2477823}"
					PuPlayer.LabelSet pBackglass,"Play2score","" & FormatNumber(Score(2),0),1,"{'mt':2,'color':3753717, 'size': 4, 'xpos': 37.5, 'xalign': 1, 'ypos': 94.5, 'yalign': 1 }"
					PuPlayer.LabelSet pBackglass,"Play2","Player 2",1,"{'mt':2,'color':2477823}"
					PuPlayer.LabelSet pBackglass,"Play3score","" & FormatNumber(Score(3),0),1,"{'mt':2,'color':3753717, 'size': 4, 'xpos': 62.5, 'xalign': 1, 'ypos': 94.5, 'yalign': 1 }"
					PuPlayer.LabelSet pBackglass,"Play3","Player 3",1,"{'mt':2,'color':2477823}"
					PuPlayer.LabelSet pBackglass,"Play4score","PLAYER 4",1,"{'mt':2,'color':4491006, 'size': 4, 'xpos': 87.5, 'xalign': 1, 'ypos': 94.5, 'yalign': 1 }"
					PuPlayer.LabelSet pBackglass,"Play4","Player 4",1,"{'mt':2,'color':2477823 }"
				End If	
			End If
		End If
	End If
end Sub

Sub IncScore(val)
curScore = curScore + val
pUpdateDMD
end Sub





'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'   Pupdmd Settings
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
	'PUPDMD Layout for each Table1
	'Setup Pages.  Note if you use fonts they must be in FONTS folder of the pupVideos\tablename\FONTS  "case sensitive exact naming fonts!"
	'*****************************************************************
	Sub pSetPageLayouts

	DIM dmddef
	DIM dmdalt
	DIM dmdscr
	DIM dmdfixed

	'labelNew <screen#>, <Labelname>, <fontName>,<size%>,<colour>,<rotation>,<xalign>,<yalign>,<xpos>,<ypos>,<PageNum>,<visible>
	'***********************************************************************'
	'<screen#>, in standard we’d set this to pDMD ( or 1)
	'<Labelname>, your name of the label. keep it short no spaces (like 8 chars) although you can call it anything really. When setting the label you will use this labelname to access the label.
	'<fontName> Windows font name, this must be exact match of OS front name. if you are using custom TTF fonts then double check the name of font names.
	'<size%>, Height as a percent of display height. 20=20% of screen height.
	'<colour>, integer value of windows color.
	'<rotation>, degrees in tenths   (900=90 degrees)
	'<xAlign>, 0= horizontal left align, 1 = center horizontal, 2= right horizontal
	'<yAlign>, 0 = top, 1 = center, 2=bottom vertical alignment
	'<xpos>, this should be 0, but if you want to ‘force’ a position you can set this. it is a % of horizontal width. 20=20% of screen width.
	'<ypos> same as xpos.
	'<PageNum> IMPORTANT… this will assign this label to this ‘page’ or group.
	'<visible> initial state of label. visible=1 show, 0 = off.



	if PuPDMDDriverType=pDMDTypeReal Then 'using RealDMD Mirroring.  **********  128x32 Real Color DMD  
		dmdalt="PKMN Pinball"
		dmdfixed="Instruction"
		dmdscr="Impact"    'main scorefont
		dmddef="Zig"

		'Page 1 (default score display)
			 PuPlayer.LabelNew pDMD,"Credits" ,dmddef,20,2477823   ,0,2,2,95,0,1,0
			 PuPlayer.LabelNew pDMD,"Play1"   ,dmdalt,21,2477823   ,1,0,0,15,0,1,0
			 PuPlayer.LabelNew pDMD,"Ball"    ,dmdalt,21,2477823   ,1,2,0,85,0,1,0
			 PuPlayer.LabelNew pDMD,"Ball2"    ,dmdalt,21,2477823   ,1,2,0,85,0,1,0
			 PuPlayer.LabelNew pDMD,"MsgScore",dmddef,45,2477823   ,0,1,0, 0,40,1,0
			 PuPlayer.LabelNew pDMD,"CurScore",dmdscr,60,2477823   ,0,1,1, 0,0,1,0
			 PuPlayer.LabelNew pDMD,"CurScore2",dmdscr,60,2477823   ,0,1,1, 0,0,1,0
			 PuPlayer.LabelNew pDMD,"CurScore3",dmdscr,60,2477823   ,0,1,1, 0,0,1,0


		'Page 2 (default Text Splash 1 Big Line)
			 PuPlayer.LabelNew pDMD,"Splash"  ,dmdalt,40,2477823,0,1,1,0,0,2,0

		'Page 3 (default Text Splash 2 and 3 Lines)
			 PuPlayer.LabelNew pDMD,"Splash3a",dmddef,30,2477823,0,1,0,0,2,3,0
			 PuPlayer.LabelNew pDMD,"Splash3b",dmdalt,30,2477823,0,1,0,0,30,3,0
			 PuPlayer.LabelNew pDMD,"Splash3c",dmdalt,25,2477823,0,1,0,0,55,3,0


		'Page 4 (2 Line Gameplay DMD)
			 PuPlayer.LabelNew pDMD,"Splash4a",dmddef,13,2477823,0,1,0,0,30,4,0
			 PuPlayer.LabelNew pDMD,"Splash4b",dmddef,10,2477823,0,1,2,0,55,4,0

		'Page 5 (3 layer large text for overlay targets function,  must you fixed width font!
			PuPlayer.LabelNew pDMD,"Back5"    ,dmdfixed,80,8421504,0,1,1,0,0,5,0
			PuPlayer.LabelNew pDMD,"Middle5"  ,dmdfixed,80,65535  ,0,1,1,0,0,5,0
			PuPlayer.LabelNew pDMD,"Flash5"   ,dmdfixed,80,65535  ,0,1,1,0,0,5,0

		'Page 6 (3 Lines for big # with two lines,  "19^Orbits^Count")
			PuPlayer.LabelNew pDMD,"Splash6a",dmddef,90,65280,0,0,0,15,1,6,0
			PuPlayer.LabelNew pDMD,"Splash6b",dmddef,50,33023,0,1,0,60,0,6,0
			PuPlayer.LabelNew pDMD,"Splash6c",dmddef,40,33023,0,1,0,60,50,6,0

		'Page 7 (Show High Scores Fixed Fonts)
			PuPlayer.LabelNew pDMD,"Splash7a",dmddef,20,8454143,0,1,0,0,2,7,0
			PuPlayer.LabelNew pDMD,"Splash7b",dmdfixed,40,33023,0,1,0,0,20,7,0
			PuPlayer.LabelNew pDMD,"Splash7c",dmdfixed,40,33023,0,1,0,0,50,7,0

			
			pDMDStartBackLoop "DMDSplash","intro1.mp4"
			dmdnote="1-shortnote.mp4"

	END IF  ' use PuPDMDDriver

	if PuPDMDDriverType=pDMDTypeLCD THEN  'Using 4:1 Standard ratio LCD PuPDMD  ************ lcd **************

		'dmddef="Impact"
		dmdalt=typefont    
		dmdfixed=typefont
		dmdscr=typefont  'main score font
		dmddef=typefont

		'Page 1 (default score display)
			PuPlayer.LabelNew pDMD,"Credits" ,dmddef,20,2477823   ,0,2,2,95,0,1,0
			PuPlayer.LabelNew pDMD,"Play1"   ,dmdalt,20,2477823   ,1,0,0,15,0,1,0
			PuPlayer.LabelNew pDMD,"Ball"    ,dmdalt,20,2477823   ,1,2,0,85,0,1,0
			PuPlayer.LabelNew pDMD,"Ball2"    ,dmdalt,20,2477823   ,1,2,0,85,0,1,0
			PuPlayer.LabelNew pDMD,"MsgScore",dmddef,45,2477823   ,0,1,0, 0,40,1,0
			PuPlayer.LabelNew pDMD,"CurScore",dmdscr,60,2477823   ,0,1,1, 0,0,1,0
			PuPlayer.LabelNew pDMD,"CurScore2",dmdscr,60,2477823   ,0,1,1, 0,0,1,0
			PuPlayer.LabelNew pDMD,"CurScore3",dmdscr,60,2477823   ,0,1,1, 0,0,1,0


		'Page 2 (default Text Splash 1 Big Line)
			PuPlayer.LabelNew pDMD,"Splash"  ,dmdalt,40,2477823,0,1,1,0,0,2,0

		'Page 3 (default Text 3 Lines)
			PuPlayer.LabelNew pDMD,"Splash3a",dmddef,30,2477823,0,1,0,0,2,3,0
			PuPlayer.LabelNew pDMD,"Splash3b",dmdalt,30,2477823,0,1,0,0,30,3,0
			PuPlayer.LabelNew pDMD,"Splash3c",dmdalt,25,2477823,0,1,0,0,57,3,0


		'Page 4 (default Text 2 Line)
			PuPlayer.LabelNew pDMD,"Splash4a",dmddef,20,2477823,0,1,0,0,20,4,0
			PuPlayer.LabelNew pDMD,"Splash4b",dmddef,15,2477823,0,1,2,0,65,4,0

		'Page 5 (3 layer large text for overlay targets function,  must you fixed width font!
			PuPlayer.LabelNew pDMD,"Back5"    ,dmdfixed,80,2477823,0,1,1,0,0,5,0
			PuPlayer.LabelNew pDMD,"Middle5"  ,dmdfixed,80,2477823  ,0,1,1,0,0,5,0
			PuPlayer.LabelNew pDMD,"Flash5"   ,dmdfixed,80,2477823  ,0,1,1,0,0,5,0

		'Page 6 (3 Lines for big # with two lines,  "19^Orbits^Count")
			PuPlayer.LabelNew pDMD,"Splash6a",dmddef,90,2477823,0,0,0,15,1,6,0
			PuPlayer.LabelNew pDMD,"Splash6b",dmddef,50,2477823,0,1,0,60,0,6,0
			PuPlayer.LabelNew pDMD,"Splash6c",dmddef,40,2477823,0,1,0,60,50,6,0

		'Page 7 (Show High Scores Fixed Fonts)
			PuPlayer.LabelNew pDMD,"Splash7a",dmddef,20,2477823,0,1,0,0,2,7,0
			PuPlayer.LabelNew pDMD,"Splash7b",dmdfixed,40,2477823,0,1,0,0,20,7,0
			PuPlayer.LabelNew pDMD,"Splash7c",dmdfixed,40,2477823,0,1,0,0,50,7,0

			pDMDStartBackLoop "DMDSplash","intro1.mp4"
			dmdnote="1-shortnote.mp4"

	END IF  ' use PuPDMDDriver

	if PuPDMDDriverType=pDMDTypeFULL THEN  'Using FULL BIG LCD PuPDMD  ************ lcd **************

		'dmddef="Impact"
		dmdalt=typefont    
		dmdfixed=typefont
		dmdscr=typefont  'main score font
		dmddef=typefont

		'Page 1 (default score display)
			PuPlayer.LabelNew pDMD,"Credits" ,dmddef,20,2477823   ,0,2,2,95,0,1,0
			PuPlayer.LabelNew pDMD,"Play1"   ,dmdalt,20,2477823   ,1,0,0,15,0,1,0
			PuPlayer.LabelNew pDMD,"Ball"    ,dmdalt,20,2477823   ,1,2,0,85,0,1,0
			PuPlayer.LabelNew pDMD,"Ball2"    ,dmdalt,20,2477823   ,1,2,0,85,0,1,0
			PuPlayer.LabelNew pDMD,"MsgScore",dmddef,45,2477823   ,0,1,0, 0,40,1,0
			PuPlayer.LabelNew pDMD,"CurScore",dmdscr,60,2477823   ,0,1,1, 0,0,1,0	
			PuPlayer.LabelNew pDMD,"CurScore2",dmdscr,60,2477823   ,0,1,1, 0,0,1,0
			PuPlayer.LabelNew pDMD,"CurScore3",dmdscr,60,2477823   ,0,1,1, 0,0,1,0

		'Page 2 (default Text Splash 1 Big Line)
			PuPlayer.LabelNew pDMD,"Splash"  ,dmdalt,20,2477823,0,1,1,0,50,2,0

		'Page 3 (default Text 3 Lines)
			PuPlayer.LabelNew pDMD,"Splash3a",dmddef,15,2477823,0,1,0,0,35,3,0
			PuPlayer.LabelNew pDMD,"Splash3b",dmdalt,10,2477823,0,1,0,0,50,3,0
			PuPlayer.LabelNew pDMD,"Splash3c",dmdalt,10,2477823,0,1,0,0,65,3,0


		'Page 4 (default Text 2 Line)
			PuPlayer.LabelNew pDMD,"Splash4a",dmddef,10,2110797,0,1,0,0,40,4,0
			PuPlayer.LabelNew pDMD,"Splash4b",dmddef,8,2110797,0,1,2,0,60,4,0

		'Page 5 (3 layer large text for overlay targets function,  must you fixed width font!
			PuPlayer.LabelNew pDMD,"Back5"    ,dmdfixed,80,2477823,0,1,1,0,0,5,0
			PuPlayer.LabelNew pDMD,"Middle5"  ,dmdfixed,80,2477823  ,0,1,1,0,0,5,0
			PuPlayer.LabelNew pDMD,"Flash5"   ,dmdfixed,80,2477823  ,0,1,1,0,0,5,0

		'Page 6 (3 Lines for big # with two lines,  "19^Orbits^Count")
			PuPlayer.LabelNew pDMD,"Splash6a",dmddef,90,2477823,0,0,0,15,1,6,0
			PuPlayer.LabelNew pDMD,"Splash6b",dmddef,50,2477823,0,1,0,60,0,6,0
			PuPlayer.LabelNew pDMD,"Splash6c",dmddef,40,2477823,0,1,0,60,50,6,0

		'Page 7 (Show High Scores Fixed Fonts)
			PuPlayer.LabelNew pDMD,"Splash7a",dmddef,20,2477823,0,1,0,0,2,7,0
			PuPlayer.LabelNew pDMD,"Splash7b",dmdfixed,40,2477823,0,1,0,0,20,7,0
			PuPlayer.LabelNew pDMD,"Splash7c",dmdfixed,40,2477823,0,1,0,0,50,7,0


			pDMDStartBackLoop "DMDSplash","intro2.mp4"
			dmdnote="2-shortnote.mp4"

	END IF  ' use PuPDMDDriver




	end Sub 'page Layouts


	'*****************************************************************
	'        PUPDMD Custom SUBS/Events for each Table1
	'     **********    MODIFY THIS SECTION!!!  ***************
	'*****************************************************************
	'

	Sub pDMDStartBall
	end Sub

	Sub pDMDGameOver
	pAttractStart
	pDMDStartBackLoop "DMDSplash","intro" & dmdver &".mp4"
	end Sub

	Sub pAttractStart
	pDMDSetPage(pDMDBlank)   'set blank text overlay page.
	pCurAttractPos=0
	pInAttract=True 'Startup in AttractMode
	pAttractNext
	end Sub

	Sub pDMDStartUP
	 'pupDMDDisplay "attract","","intro" & dmdver &".mp4",71,0,10 
	 pInAttract=true  ' will startup attract after startup video
	end Sub

	Sub pDMDStartGame
'		pDMDStartBackLoop "Attract loop","ATTRACT_LOOP1.mp4"
'		pupDMDDisplay "attract","",dmdver & "-background.mp4",71,0,1
	 'pupDMDDisplay "attract","",dmdver & "-background.mp4",3,0,20
	 'PuPlayer.SetLoop 3,1  '3????
	 pInAttract=false  ' will startup attract after startup video
	end Sub

	'********************** gets called auto each page next and timed already in DMD_Timer.  make sure you use pupDMDDisplay or it wont advance auto.
	Sub pAttractNext
'	pCurAttractPos=pCurAttractPos+1

'	  Select Case pCurAttractPos

'	  Case 1 pupDMDDisplay "Attract loop","","ATTRACT_LOOP1.mp4",71,0,1
'	  Case 2 pupDMDDisplay "Attract loop","ATTRACT_LOOP2.mp4","",71,0,10
	'  Case 3 pupDMDDisplay "attract","Attract^3","",2,0,10
	'  Case 4 pupDMDDisplay "attract","Attract^4","",3,1,10
	'  Case 5 pupDMDDisplay "attract","Attract^5","",1,0,10
	'  Case 6 pupDMDDisplay "attract","Attract^6","",3,1,10
	'  Case 7 pupDMDDisplay "attract","Attract^7","",2,0,10
	'  Case 8 pupDMDDisplay "attract","Attract^8","",1,0,10
	'  Case 9 pupDMDDisplay "attract","Attract^9","",1,1,10
	'  Case 10 pupDMDDisplay "attract","Attract^10","",3,1,10
'	  Case Else
'		pCurAttractPos=0
'		pAttractNext 'reset to beginning
'	  end Select

	end Sub


	'************************ called during gameplay to update Scores ***************************
Sub pDMDUpdateScores  'call this ONLY on timer 300ms is good enough
	if pDMDCurPage <> pScores then Exit Sub
	If HideOverlay = False Then 
		If CurrentMissionFlag(CurrentPlayer) = 2  Then
			If BlackKnightRetro(CurrentPlayer) = 3 Then
				puPlayer.LabelSet pDMD,"CurScore2","" & "" & FormatNumber(Score(CurrentPlayer),0),1,""
			End If
			If BlackKnightRetro(CurrentPlayer) = 1 Then
				puPlayer.LabelSet pDMD,"CurScore3","" & "" & FormatNumber(Score(CurrentPlayer),0) & " ",1,""
			End If
				puPlayer.LabelSet pDMD,"CurScore","",1,""
				puPlayer.LabelSet pDMD,"Play1","",1,""
			if (bpgcurrent - BallsRemaining(CurrentPlayer) + 1) < 6 Then					'updated for 5 Balls
				puPlayer.LabelSet pDMD,"Ball","",1,""
				puPlayer.LabelSet pDMD,"Ball2","",1,""
			else
				puPlayer.LabelSet pDMD,"Ball","",1,""
				puPlayer.LabelSet pDMD,"Ball2","",1,""
			End if
		Else
			puPlayer.LabelSet pDMD,"CurScore","" & "" & FormatNumber(Score(CurrentPlayer),0),1,""
			puPlayer.LabelSet pDMD,"CurScore2","",1,""
			puPlayer.LabelSet pDMD,"CurScore3","",1,""
			puPlayer.LabelSet pDMD,"Play1","play " & CurrentPlayer,1,""
			if (bpgcurrent - BallsRemaining(CurrentPlayer) + 1) < 6 Then					'updated for 5 Balls
				puPlayer.LabelSet pDMD,"Ball","ball "  & (bpgcurrent - BallsRemaining(CurrentPlayer)) + 1,1,""
				puPlayer.LabelSet pDMD,"Ball2","",1,""
			else
				puPlayer.LabelSet pDMD,"Ball","ball ",1,""
				puPlayer.LabelSet pDMD,"Ball2","",1,""
			End if

		End If
	Else
		' in comment but Else condition can be removed
	End If
End Sub

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  High Scores
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  



Sub ResetValue				' Load Score With Default Value
	SaveValue TableName, "HighScore1", "750000000"
	SaveValue TableName, "HighScore1Name", "SSR"
	SaveValue TableName, "HighScore2", "550000000"
	SaveValue TableName, "HighScore2Name", "TSX"
	SaveValue TableName, "HighScore3", "400000000"
	SaveValue TableName, "HighScore3Name", "JMR"
	SaveValue TableName, "HighScore4", "300000000"
	SaveValue TableName, "HighScore4Name", "J C"
	SaveValue TableName, "HighScore5", "250000000"
	SaveValue TableName, "HighScore5Name", "D K"
End Sub

Sub PutSternScore (CheckScore,CheckScoreName)
		hschecker = 1
		hs4 = HighScore(4)
 		hs3 = HighScore(3)
		hs2 = HighScore(2)
		hs1 = HighScore(1)
		hs0 = HighScore(0)
		hsn4 = HighScoreName(4)
		hsn3 = HighScoreName(3)
		hsn2 = HighScoreName(2)
		hsn1 = HighScoreName(1)
		hsn0 = HighScoreName(0)
		If CheckScore > hs0 Then
			HighScore(0) = CheckScore
			HighScoreName(0) = CheckScoreName	
			HighScore(1) = hs0
			HighScoreName(1) = hsn0	
			HighScore(2) = hs1
			HighScoreName(2) = hsn1	
			HighScore(3) = hs2
			HighScoreName(3) = hsn2
			HighScore(4) = hs3
			HighScoreName(4) = hsn3
		ElseIf CheckScore = hs0 And CheckScoreName = hsn0 Then
			'Nothing
		ElseIf CheckScore > hs1 Then
			HighScore(0) = hs0
			HighScoreName(0) = hsn0	
			HighScore(1) = CheckScore
			HighScoreName(1) = CheckScoreName	
			HighScore(2) = hs1
			HighScoreName(2) = hsn1	
			HighScore(3) = hs2
			HighScoreName(3) = hsn2
			HighScore(4) = hs3
			HighScoreName(4) = hsn3
		ElseIf CheckScore = hs1 And CheckScoreName = hsn1 Then
			'Nothing
		ElseIf CheckScore > hs2 Then
			HighScore(0) = hs0
			HighScoreName(0) = hsn0	
			HighScore(1) = hs1
			HighScoreName(1) = hsn1	
			HighScore(2) = CheckScore
			HighScoreName(2) = CheckScoreName	
			HighScore(3) = hs2
			HighScoreName(3) = hsn2
			HighScore(4) = hs3
			HighScoreName(4) = hsn3
		ElseIf CheckScore = hs2 And CheckScoreName = hsn2 Then
			'Nothing
		ElseIf CheckScore > hs3 Then
			HighScore(0) = hs0
			HighScoreName(0) = hsn0	
			HighScore(1) = hs1
			HighScoreName(1) = hsn1	
			HighScore(2) = hs2
			HighScoreName(2) = hsn2	
			HighScore(3) = CheckScore
			HighScoreName(3) = CheckScoreName
			HighScore(4) = hs3
			HighScoreName(4) = hsn3
		ElseIf CheckScore = hs3 And CheckScoreName = hsn3 Then
			'Nothing
		ElseIf CheckScore > hs4 Then
			HighScore(0) = hs0
			HighScoreName(0) = hsn0	
			HighScore(1) = hs1
			HighScoreName(1) = hsn1	
			HighScore(2) = hs2
			HighScoreName(2) = hsn2	
			HighScore(3) = hs3
			HighScoreName(3) = hsn3
			HighScore(4) = CheckScore
			HighScoreName(4) = CheckScoreName
		ElseIf CheckScore = hs4 And CheckScoreName = hsn4 Then
			'Nothing
		Else
			HighScore(0) = hs0
			HighScoreName(0) = hsn0	
			HighScore(1) = hs1
			HighScoreName(1) = hsn1	
			HighScore(2) = hs2
			HighScoreName(2) = hsn2	
			HighScore(3) = hs3
			HighScoreName(3) = hsn3
			HighScore(4) = hs4
			HighScoreName(4) = hsn4
		End If

		savehs
	End Sub

Sub Loadhs
	Dim x
	x = LoadValue(TableName, "HighScore1")
	If(x <> "") Then HighScore(0) = CDbl(x) Else HighScore(0) = 750000000 End If

	x = LoadValue(TableName, "HighScore1Name")
	If(x <> "") Then HighScoreName(0) = x Else HighScoreName(0) = "SSR" End If

	x = LoadValue(TableName, "HighScore2")
	If(x <> "") then HighScore(1) = CDbl(x) Else HighScore(1) = 550000000 End If

	x = LoadValue(TableName, "HighScore2Name")
	If(x <> "") then HighScoreName(1) = x Else HighScoreName(1) = "TSX" End If

	x = LoadValue(TableName, "HighScore3")
	If(x <> "") then HighScore(2) = CDbl(x) Else HighScore(2) = 400000000 End If

	x = LoadValue(TableName, "HighScore3Name")
	If(x <> "") then HighScoreName(2) = x Else HighScoreName(2) = "JMR" End If

	x = LoadValue(TableName, "HighScore4")
	If(x <> "") then HighScore(3) = CDbl(x) Else HighScore(3) = 300000000 End If

	x = LoadValue(TableName, "HighScore4Name")
	If(x <> "") then HighScoreName(3) = x Else HighScoreName(3) = "J C" End If

	x = LoadValue(TableName, "HighScore5")
	If(x <> "") then HighScore(4) = CDbl(x) Else HighScore(4) = 250000000 End If

	x = LoadValue(TableName, "HighScore5Name")
	If(x <> "") then HighScoreName(4) = x Else HighScoreName(4) = "D K" End If

	x = LoadValue(TableName, "Credits")
	If(x <> "") then Credits = CInt(x) Else Credits = 0 End If

	x = LoadValue(TableName, "TotalGamesPlayed")
	If(x <> "") then TotalGamesPlayed = CDbl(x) Else TotalGamesPlayed = 0 End If

	x = LoadValue(TableName, "BKSOR01")
	If(x <> "") then HighScoreComboChampionName = x Else HighScoreComboChampionName = "" End If

	x = LoadValue(TableName, "BKSOR02")
	If(x <> "") then HighScoreComboChampion = CDbl(x) Else HighScoreComboChampion = 0 End If

	x = LoadValue(TableName, "BKSOR03")
	If(x <> "") then HighScoreKnightChampionName = x Else HighScoreKnightChampionName = "" End If

	x = LoadValue(TableName, "BKSOR04")
	If(x <> "") then HighScoreKnightChampion = CDbl(x) Else HighScoreKnightChampion = 0 End If

	x = LoadValue(TableName, "BKSOR05")
	If(x <> "") then HighScoreBlackCastleChampionName = x Else HighScoreBlackCastleChampionName = "" End If

	x = LoadValue(TableName, "BKSOR06")
	If(x <> "") then HighScoreBlackCastleChampion = CDbl(x) Else HighScoreBlackCastleChampion = 0 End If

	x = LoadValue(TableName, "BKSOR07")
	If(x <> "") then HighScoreLoopChampionName = x Else HighScoreLoopChampionName = "" End If

	x = LoadValue(TableName, "BKSOR08")
	If(x <> "") then HighScoreLoopChampion = CDbl(x) Else HighScoreLoopChampion = 0 End If

	x = LoadValue(TableName, "BKSOR09")
	If(x <> "") then HighScoreWarChampionName = x Else HighScoreWarChampionName = "" End If

	x = LoadValue(TableName, "BKSOR10")
	If(x <> "") then HighScoreWarChampion = CDbl(x) Else HighScoreWarChampion = 0 End If

	x = LoadValue(TableName, "BKSOR11")
	If(x <> "") then HighScoreBonusChampionName = x Else HighScoreBonusChampionName = "" End If

	x = LoadValue(TableName, "BKSOR12")
	If(x <> "") then HighScoreBonusChampion = CDbl(x) Else HighScoreBonusChampion = 0 End If

	x = LoadValue(TableName, "BKSOR13")
	If(x <> "") then HighScoreTripleKnightsMBChampionName = x Else HighScoreTripleKnightsMBChampionName = "" End If

	x = LoadValue(TableName, "BKSOR14")
	If(x <> "") then HighScoreTripleKnightsMBChampion = CDbl(x) Else HighScoreTripleKnightsMBChampion = 0 End If

	x = LoadValue(TableName, "BKSOR15")
	If(x <> "") then HighScoreCatapultMBChampionName = x Else HighScoreCatapultMBChampionName = "" End If

	x = LoadValue(TableName, "BKSOR16")
	If(x <> "") then HighScoreCatapultMBChampion = CDbl(x) Else HighScoreCatapultMBChampion = 0 End If

	x = LoadValue(TableName, "BKSOR17")
	If(x <> "") then HighScoreMoltenFireChampionName = x Else HighScoreMoltenFireChampionName = "" End If

	x = LoadValue(TableName, "BKSOR18")
	If(x <> "") then HighScoreMoltenFireChampion = CDbl(x) Else HighScoreMoltenFireChampion = 0 End If

	x = LoadValue(TableName, "BKSOR19")
	If(x <> "") then HighScoreDeepFreezeChampionName = x Else HighScoreDeepFreezeChampionName = "" End If

	x = LoadValue(TableName, "BKSOR20")
	If(x <> "") then HighScoreDeepFreezeChampion = CDbl(x) Else HighScoreDeepFreezeChampion = 0 End If

	x = LoadValue(TableName, "BKSOR21")
	If(x <> "") then HighScoreMudBogChampionName = x Else HighScoreMudBogChampionName = "" End If

	x = LoadValue(TableName, "BKSOR22")
	If(x <> "") then HighScoreMudBogChampion = CDbl(x) Else HighScoreMudBogChampion = 0 End If

	x = LoadValue(TableName, "BKSOR23")
	If(x <> "") then HighScoreWickedCavernChampionName = x Else HighScoreWickedCavernChampionName = "" End If

	x = LoadValue(TableName, "BKSOR24")
	If(x <> "") then HighScoreWickedCavernChampion = CDbl(x) Else HighScoreWickedCavernChampion = 0 End If

	x = LoadValue(TableName, "BKSOR25")
	If(x <> "") then HighScoreBurningSandsChampionName = x Else HighScoreBurningSandsChampionName = "" End If

	x = LoadValue(TableName, "BKSOR26")
	If(x <> "") then HighScoreBurningSandsChampion = CDbl(x) Else HighScoreBurningSandsChampion = 0 End If

	x = LoadValue(TableName, "BKSOR27")
	If(x <> "") then HighScoreSuperChampionName = x Else HighScoreSuperChampionName = "" End If

	x = LoadValue(TableName, "BKSOR28")
	If(x <> "") then HighScoreSuperChampion = CDbl(x) Else HighScoreSuperChampion = 0 End If

	If hschecker = 0 Then
	checkorder
	UpdateSternScore.Enabled = True
	End If
	Savehs
End Sub

Sub UpdateSternScore_time()
ScoreSternupdated = ScoreSternupdated + 1
If ScoreSternupdated = 1 Then PutSternScore 750000000,"SSR" End If
If ScoreSternupdated = 2 Then PutSternScore 550000000,"TSX" End If
If ScoreSternupdated = 3 Then PutSternScore 400000000,"JMR" End If
If ScoreSternupdated = 4 Then PutSternScore 300000000,"J C" End If
If ScoreSternupdated = 5 Then PutSternScore 250000000,"D K" : UpdateSternScore.Enabled = False: End If


End Sub

	Sub checkorder
		hschecker = 1
		hs4 = HighScore(4)
 		hs3 = HighScore(3)
		hs2 = HighScore(2)
		hs1 = HighScore(1)
		hs0 = HighScore(0)
		hsn4 = HighScoreName(4)
		hsn3 = HighScoreName(3)
		hsn2 = HighScoreName(2)
		hsn1 = HighScoreName(1)
		hsn0 = HighScoreName(0)
		If hs4 > hs0 Then
			HighScore(0) = hs4
			HighScoreName(0) = hsn4	
			HighScore(1) = hs0
			HighScoreName(1) = hsn0	
			HighScore(2) = hs1
			HighScoreName(2) = hsn1	
			HighScore(3) = hs2
			HighScoreName(3) = hsn2
			HighScore(4) = hs3
			HighScoreName(4) = hsn3

		ElseIf hs4 > hs1 Then
			HighScore(0) = hs0
			HighScoreName(0) = hsn0	
			HighScore(1) = hs4
			HighScoreName(1) = hsn4	
			HighScore(2) = hs1
			HighScoreName(2) = hsn1	
			HighScore(3) = hs2
			HighScoreName(3) = hsn2
			HighScore(4) = hs3
			HighScoreName(4) = hsn3
		ElseIf hs4 > hs2 Then
			HighScore(0) = hs0
			HighScoreName(0) = hsn0	
			HighScore(1) = hs1
			HighScoreName(1) = hsn1	
			HighScore(2) = hs4
			HighScoreName(2) = hsn4	
			HighScore(3) = hs2
			HighScoreName(3) = hsn2
			HighScore(4) = hs3
			HighScoreName(4) = hsn3
		ElseIf hs4 > hs3 Then
			HighScore(0) = hs0
			HighScoreName(0) = hsn0	
			HighScore(1) = hs1
			HighScoreName(1) = hsn1	
			HighScore(2) = hs2
			HighScoreName(2) = hsn2	
			HighScore(3) = hs4
			HighScoreName(3) = hsn4
			HighScore(4) = hs3
			HighScoreName(4) = hsn3
		ElseIf hs4 < hs3 Then
			HighScore(0) = hs0
			HighScoreName(0) = hsn0	
			HighScore(1) = hs1
			HighScoreName(1) = hsn1	
			HighScore(2) = hs2
			HighScoreName(2) = hsn2	
			HighScore(3) = hs3
			HighScoreName(3) = hsn3
			HighScore(4) = hs4
			HighScoreName(4) = hsn4
		End If

'		savehs								'Commented because saved after in 'sub HighScoreCommitName()
	End Sub

	Sub SaveCredits
		SaveValue TableName, "Credits", Credits
	End Sub

Sub Savehs
	SaveValue TableName, "HighScore1", HighScore(0)
	SaveValue TableName, "HighScore1Name", HighScoreName(0)
	SaveValue TableName, "HighScore2", HighScore(1)
	SaveValue TableName, "HighScore2Name", HighScoreName(1)
	SaveValue TableName, "HighScore3", HighScore(2)
	SaveValue TableName, "HighScore3Name", HighScoreName(2)
	SaveValue TableName, "HighScore4", HighScore(3)
	SaveValue TableName, "HighScore4Name", HighScoreName(3)
	SaveValue TableName, "HighScore5", HighScore(4)
	SaveValue TableName, "HighScore5Name", HighScoreName(4)
	SaveValue TableName, "Credits", Credits
	SaveValue TableName, "BKSOR01", HighScoreComboChampionName
	SaveValue TableName, "BKSOR02", HighScoreComboChampion
	SaveValue TableName, "BKSOR03", HighScoreKnightChampionName
	SaveValue TableName, "BKSOR04", HighScoreKnightChampion
	SaveValue TableName, "BKSOR05", HighScoreBlackCastleChampionName
	SaveValue TableName, "BKSOR06", HighScoreBlackCastleChampion
	SaveValue TableName, "BKSOR07", HighScoreLoopChampionName
	SaveValue TableName, "BKSOR08", HighScoreLoopChampion
	SaveValue TableName, "BKSOR09", HighScoreWarChampionName
	SaveValue TableName, "BKSOR10", HighScoreWarChampion
	SaveValue TableName, "BKSOR11", HighScoreBonusChampionName
	SaveValue TableName, "BKSOR12", HighScoreBonusChampion
	SaveValue TableName, "BKSOR13", HighScoreTripleKnightsMBChampionName
	SaveValue TableName, "BKSOR14", HighScoreTripleKnightsMBChampion
	SaveValue TableName, "BKSOR15", HighScoreCatapultMBChampionName
	SaveValue TableName, "BKSOR16", HighScoreCatapultMBChampion
	SaveValue TableName, "BKSOR17", HighScoreMoltenFireChampionName
	SaveValue TableName, "BKSOR18", HighScoreMoltenFireChampion
	SaveValue TableName, "BKSOR19", HighScoreDeepFreezeChampionName
	SaveValue TableName, "BKSOR20", HighScoreDeepFreezeChampion
	SaveValue TableName, "BKSOR21", HighScoreMudBogChampionName
	SaveValue TableName, "BKSOR22", HighScoreMudBogChampion
	SaveValue TableName, "BKSOR23", HighScoreWickedCavernChampionName
	SaveValue TableName, "BKSOR24", HighScoreWickedCavernChampion
	SaveValue TableName, "BKSOR25", HighScoreBurningSandsChampionName
	SaveValue TableName, "BKSOR26", HighScoreBurningSandsChampion
	SaveValue TableName, "BKSOR27", HighScoreSuperChampionName
	SaveValue TableName, "BKSOR28", HighScoreSuperChampion
End Sub


Sub ResetHs
		SaveValue TableName, "HighScore1", HighScore(0)
		SaveValue TableName, "HighScore1Name", HighScoreName(0)
		SaveValue TableName, "HighScore2", HighScore(1)
		SaveValue TableName, "HighScore2Name", HighScoreName(1)
		SaveValue TableName, "HighScore3", HighScore(2)
		SaveValue TableName, "HighScore3Name", HighScoreName(2)
		SaveValue TableName, "HighScore4", HighScore(3)
		SaveValue TableName, "HighScore4Name", HighScoreName(3)
		SaveValue TableName, "HighScore5", HighScore(4)
		SaveValue TableName, "HighScore5Name", HighScoreName(4)
		SaveValue TableName, "Credits", Credits
		SaveValue TableName, "BKSOR01", ""
		SaveValue TableName, "BKSOR02", "0"
		SaveValue TableName, "BKSOR03", ""
		SaveValue TableName, "BKSOR04", "0"
		SaveValue TableName, "BKSOR05", ""
		SaveValue TableName, "BKSOR06", "0"
		SaveValue TableName, "BKSOR07", ""
		SaveValue TableName, "BKSOR08", "0"
		SaveValue TableName, "BKSOR09", ""
		SaveValue TableName, "BKSOR10", "0"
		SaveValue TableName, "BKSOR11", ""
		SaveValue TableName, "BKSOR12", "0"
		SaveValue TableName, "BKSOR13", ""
		SaveValue TableName, "BKSOR14", "0"
		SaveValue TableName, "BKSOR15", ""
		SaveValue TableName, "BKSOR16", "0"
		SaveValue TableName, "BKSOR17", ""
		SaveValue TableName, "BKSOR18", "0"
		SaveValue TableName, "BKSOR19", ""
		SaveValue TableName, "BKSOR20", "0"
		SaveValue TableName, "BKSOR21", ""
		SaveValue TableName, "BKSOR22", "0"
		SaveValue TableName, "BKSOR23", ""
		SaveValue TableName, "BKSOR24", "0"
		SaveValue TableName, "BKSOR25", ""
		SaveValue TableName, "BKSOR26", "0"
		SaveValue TableName, "BKSOR27", ""
		SaveValue TableName, "BKSOR28", "0"
	End Sub


	Sub Savegp
		SaveValue TableName, "TotalGamesPlayed", TotalGamesPlayed
		'vpmtimer.addtimer 1000, "Loadhs'"
'		Loadhs
	End Sub

Sub CheckHighscore (PlayerCheck)
	
	BallControlTimer.Enabled = False
	DrainHited.Enabled = False
	UnblockDrainHited.Enabled = False
	LastChanceButtonPush(CurrentPlayer) = False								'This line is Add, because, the Last chance is started during the matching score  (when the sub CheckHighscore is used the game is finish for all players)
	tmpScore = Score(PlayerCheck)
	osbtempscore = Score(PlayerCheck)
'/**/
	PlayerCheckCurrent = PlayerCheck
	tmpComboChampion = ScoreComboChampion(PlayerCheck)
	tmpKnightChampion = ScoreKnightChampion(PlayerCheck)
	tmpBlackCastleChampion = ScoreBlackCastleChampion(PlayerCheck)
	tmpLoopChampion = ScoreLoopChampion(PlayerCheck)
	tmpWarChampion = ScoreWarChampion(PlayerCheck)
	tmpBonusChampion = ScoreBonusChampion(PlayerCheck)
	tmpTripleKnightsMBChampion = ScoreTripleKnightsMBChampion(PlayerCheck)
	tmpCatapultMBChampion = ScoreCatapultMBChampion(PlayerCheck)
	tmpMoltenFireChampion = ScoreMoltenFireChampion(PlayerCheck)
	tmpDeepFreezeChampion = ScoreDeepFreezeChampion(PlayerCheck)
	tmpMudBogChampion = ScoreMudBogChampion(PlayerCheck)
	tmpWickedCavernChampion = ScoreWickedCavernChampion(PlayerCheck)
	tmpBurningSandsChampion = ScoreBurningSandsChampion(PlayerCheck)
	tmpSuperChampion = ScoreSuperChampion(PlayerCheck)
'/**/
	NewRecord = False
	If tmpScore > HighScore(4) Then		
		NewRecord = True
	ElseIf tmpComboChampion > HighScoreComboChampion Then
		NewRecord = True
	ElseIf tmpKnightChampion > HighScoreKnightChampion Then
		NewRecord = True
	ElseIf tmpBlackCastleChampion > HighScoreBlackCastleChampion Then
		NewRecord = True
	ElseIf tmpLoopChampion > HighScoreLoopChampion Then
		NewRecord = True
	ElseIf tmpWarChampion > HighScoreWarChampion Then
		NewRecord = True
	ElseIf tmpBonusChampion > HighScoreBonusChampion Then
		NewRecord = True
	ElseIf tmpTripleKnightsMBChampion > HighScoreTripleKnightsMBChampion Then
		NewRecord = True
	ElseIf tmpCatapultMBChampion > HighScoreCatapultMBChampion Then
		NewRecord = True
	ElseIf tmpMoltenFireChampion > HighScoreMoltenFireChampion Then
		NewRecord = True
	ElseIf tmpDeepFreezeChampion > HighScoreDeepFreezeChampion Then
		NewRecord = True
	ElseIf tmpMudBogChampion > HighScoreMudBogChampion Then
		NewRecord = True
	ElseIf tmpWickedCavernChampion > HighScoreWickedCavernChampion Then
		NewRecord = True
	ElseIf tmpBurningSandsChampion > HighScoreBurningSandsChampion Then
		NewRecord = True
	ElseIf tmpSuperChampion > HighScoreSuperChampion Then
		NewRecord = True
'/**/
'	Else

'		osbtemp = osbdefinit
'		if osbkey="" Then
'		Else
''			SubmitOSBScore
'		end If
''		EndOfBallComplete()
''		EndOfGame
	End If

'/**/


	If NewRecord = True Then
		playclear pBackglass
'		AwardSpecial		
'		HighScore(4) = tmp
		'enter player's name
		TempoDisplay.Enabled = True
		VideoNewRecord
	Else
		If PlayerCheckCurrent > 1 Then							'Check If There is another Champion (Check player 4 to 1 )
			PlayerCheckCurrent = PlayerCheckCurrent - 1
			CheckHighScore PlayerCheckCurrent
		Else
'			EndOfBallComplete()
			EndOfGame
			PuPlayer.LabelSet pBackglass,"HighScore","",1,""
			PuPlayer.LabelSet pBackglass,"HighScore2","",1,""
			PuPlayer.LabelSet pBackglass,"HighScoreL1","",1,""
			PuPlayer.LabelSet pBackglass,"HighScoreL2"," ",1,""
			PuPlayer.LabelSet pBackglass,"HighScoreL3"," ",1,""
			PuPlayer.LabelSet pBackglass,"HighScoreL4"," ",1,""
			hsbModeActive = False
		End If
	End If
'/**/
End Sub





	Sub HighScoreEntryInit()
		hsbModeActive = True
'		PlaySound "vo_enteryourinitials"				'modify later

		hsEnteredDigits(1) = "A"
		hsEnteredDigits(2) = " "
		hsEnteredDigits(3) = " "

		hsCurrentDigit = 1
		'PuPlayer.playlistplayex pCallouts,"audiocallouts","yougotahighscore.mp3",vovol,1
		playmedia "yougotahighscore.mp3","audiocallouts",pCallouts,"",2000,"",1,1  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'		pupDMDDisplay "-","You Got a^High Score",dmdnote,3,0,10
		'chilloutthemusic
		'PuPlayer.playpause 4
		playclear pMusic
		'playmedia "hs.mp3","audiomultiballs",pAudio,"cineon",10000,"",1,0,1  // (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority))
		playmedia "hs.mp3","audiomultiballs",pAudio,0,0,"",1,1

		'playmedia "hs.mp3","audiomultiballs",pAudio,"cineon",10000,"",1,0,1  // (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		playmedia "highscore.mp4","videoscenes",pBackglass,0,0,"",1,2
		PuPlayer.SetLoop 2,1
		Playsound SoundFXDOF("knocker",136,DOFPulse,DOFKnocker)
		HighScoreDisplayName()
		HighScorelabels	
	End Sub

	' flipper moving around the letters

	Sub EnterHighScoreKey(keycode)
		If keycode = LeftFlipperKey Then
'			Playsound "Sound-0x0010"			'modify Later
			PlaySound "Sound-0x0010", 0, SongVolume		'modify Later
				If hsletter = 0 Then
					hsletter = 26
				Else
					hsLetter = hsLetter - 1
				End If
				HighScoreDisplayName()
		End If

		If keycode = RightFlipperKey Then
'			Playsound "Sound-0x0019"						'modify Later
			PlaySound "Sound-0x0019", 0, SongVolume		'modify Later
				If hsletter = 26 Then
					hsletter = 0
				Else
					hsLetter = hsLetter + 1
				End If
				HighScoreDisplayName()
		End If

		If keycode = StartGameKey or keycode = PlungerKey Then
'			PlaySound "Sound-0x0008"				'modify Later
			PlaySound "Sound-0x0008", 0, SongVolume				'modify Later
				If hsCurrentDigit = 3 Then
					If hsletter = 0 Then
						hsCurrentDigit = hsCurrentDigit -1
					Else
						assignletter
						'vpmtimer.addtimer 700, "HighScoreCommitName()'"
						HighScoreCommitName()
						playclear pBackglass
					End If
				End If
				If hsCurrentDigit < 3 Then
					If hsletter = 0 Then
						If hsCurrentDigit = 1 Then
						Else
							hsCurrentDigit = hsCurrentDigit -1
						End If
					Else
						assignletter
						hsCurrentDigit = hsCurrentDigit + 1
						HighScoreDisplayName()

					End If
				End If
		End if
	End Sub

	Sub assignletter
		if hscurrentdigit = 1 Then
			hsdigit = 1
		End If
		if hscurrentdigit = 2 Then
			hsdigit = 2
		End If
		if hscurrentdigit = 3 Then
			hsdigit = 3
		End If
		If hsletter = 1 Then 
			hsEnteredDigits(hsdigit) = "A"
		End If
		If hsletter = 2 Then 
			hsEnteredDigits(hsdigit) = "B"
		End If
		If hsletter = 3 Then 
			hsEnteredDigits(hsdigit) = "C"
		End If
		If hsletter = 4 Then 
			hsEnteredDigits(hsdigit) = "D"
		End If
		If hsletter = 5 Then 
			hsEnteredDigits(hsdigit) = "E"
		End If
		If hsletter = 6 Then 
			hsEnteredDigits(hsdigit) = "F"
		End If
		If hsletter = 7 Then 
			hsEnteredDigits(hsdigit) = "G"
		End If
		If hsletter = 8 Then 
			hsEnteredDigits(hsdigit) = "H"
		End If
		If hsletter = 9 Then 
			hsEnteredDigits(hsdigit) = "I"
		End If
		If hsletter = 10 Then 
			hsEnteredDigits(hsdigit) = "J"
		End If
		If hsletter = 11 Then 
			hsEnteredDigits(hsdigit) = "K"
		End If
		If hsletter = 12 Then 
			hsEnteredDigits(hsdigit) = "L"
		End If
		If hsletter = 13 Then 
			hsEnteredDigits(hsdigit) = "M"
		End If
		If hsletter = 14 Then 
			hsEnteredDigits(hsdigit) = "N"
		End If
		If hsletter = 15 Then 
			hsEnteredDigits(hsdigit) = "O"
		End If
		If hsletter = 16 Then 
			hsEnteredDigits(hsdigit) = "P"
		End If
		If hsletter = 17 Then 
			hsEnteredDigits(hsdigit) = "Q"
		End If
		If hsletter = 18 Then 
			hsEnteredDigits(hsdigit) = "R"
		End If
		If hsletter = 19 Then 
			hsEnteredDigits(hsdigit) = "S"
		End If
		If hsletter = 20 Then 
			hsEnteredDigits(hsdigit) = "T"
		End If
		If hsletter = 21 Then 
			hsEnteredDigits(hsdigit) = "U"
		End If
		If hsletter = 22 Then 
			hsEnteredDigits(hsdigit) = "V"
		End If
		If hsletter = 23 Then 
			hsEnteredDigits(hsdigit) = "W"
		End If
		If hsletter = 24 Then 
			hsEnteredDigits(hsdigit) = "X"
		End If
		If hsletter = 25 Then 
			hsEnteredDigits(hsdigit) = "Y"
		End If
		If hsletter = 26 Then 
			hsEnteredDigits(hsdigit) = "Z"
		End If

	End Sub

Sub TempoDisplay_Timer()
	TempoDisplay.Enabled = False
	HighScoreEntryInit()
'	HighScorelabels
	HideOverlay = True
	PuPlayer.LabelSet pBackglass,"AddScoreChest","",1,"{'mt':2,'color':28671, 'size': 5, 'xpos': 14, 'xalign': 1, 'ypos': 69, 'yalign': 1}"
	PuPlayer.LabelSet pBackglass,"TimerSuperMode","",1,"{'mt':2,'color':16777215, 'size': 6, 'xpos': 5, 'xalign': 1, 'ypos': 9, 'yalign': 1}"
	puPlayer.LabelSet pBackglass,"LastChanceBallToLock","",1,""
	puPlayer.LabelSet pBackglass,"LastChanceTime","",1,""
	puPlayer.LabelSet pBackglass,"LastChanceJackpot","",1,""
	pSplashCommentDisplayed "",3,255
	pSplashCommentDisplayed2 "",3,40
	pSplashAddScoreDisplayed "",3,255
	pSplashAddScoreDisplayed2 "",3,40
	
End Sub

	Sub HighScorelabels
		PuPlayer.LabelSet pBackglass,"HighScore","Player "&PlayerCheckCurrent&" - Enter Initial",1,"{'mt':2,'color':30719, 'size': 8, 'xpos': 50, 'xalign': 1, 'ypos': 16, 'yalign': 1}"
		PuPlayer.LabelSet pBackglass,"HighScore2","grand champion",1,"{'mt':2,'color':30719, 'size': 8, 'xpos': 50, 'xalign': 1, 'ypos': 36, 'yalign': 1}"
		PuPlayer.LabelSet pBackglass,"HighScoreL1","A",1,"{'mt':2,'color':30719, 'size': 10, 'xpos': 42, 'xalign': 1, 'ypos': 65, 'yalign': 1}"
		PuPlayer.LabelSet pBackglass,"HighScoreL2","",1,"{'mt':2,'color':30719, 'size': 10, 'xpos': 50, 'xalign': 1, 'ypos': 65, 'yalign': 1}"
		PuPlayer.LabelSet pBackglass,"HighScoreL3","",1,"{'mt':2,'color':30719, 'size': 10, 'xpos': 58, 'xalign': 1, 'ypos': 65, 'yalign': 1}"
'		PuPlayer.LabelSet pBackglass,"HighScoreL4",Score(CurrentPlayer),1,"{'mt':2,'color':2477823, 'size': 6, 'xpos': 10.5, 'xalign': 0, 'ypos': 58.8, 'yalign': 1}"
		hsletter = 1
		HideOverlay = True
	End Sub

	Sub HighScoreDisplayName()

		Select case hsLetter
		Case 0
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","<",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","<",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","<",1,""
		Case 1
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","A",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","A",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","A",1,""
		Case 2
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","B",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","B",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","B",1,""
		Case 3
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","C",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","C",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","C",1,""
		Case 4
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","D",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","D",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","D",1,""
		Case 5
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","E",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","E",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","E",1,""
		Case 6
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","F",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","F",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","F",1,""
		Case 7
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","G",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","G",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","G",1,""
		Case 8
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","H",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","H",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","H",1,""
		Case 9
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","I",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","I",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","I",1,""
		Case 10
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","J",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","J",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","J",1,""
		Case 11
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","K",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","K",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","K",1,""
		Case 12
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","L",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","L",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","L",1,""
		Case 13
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","M",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","M",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","M",1,""
		Case 14
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","N",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","N",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","N",1,""
		Case 15
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","O",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","O",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","O",1,""
		Case 16
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","P",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","P",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","P",1,""
		Case 17
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","Q",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","Q",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","Q",1,""
		Case 18
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","R",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","R",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","R",1,""
		Case 19
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","S",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","S",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","S",1,""
		Case 20
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","T",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","T",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","T",1,""
		Case 21
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","U",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","U",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","U",1,""
		Case 22
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","V",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","V",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","V",1,""
		Case 23
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","W",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","W",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","W",1,""
		Case 24
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","X",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","X",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","X",1,""
		Case 25
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","Y",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","Y",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","Y",1,""
		Case 26
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pBackglass,"HighScoreL1","Z",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pBackglass,"HighScoreL2","Z",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pBackglass,"HighScoreL3","Z",1,""
		End Select
	End Sub

	' post the high score letters


Sub HighScoreCommitName()
		'PuPlayer.playresume 4
		playclear pBackglass
		PuPlayer.SetLoop 2,0
		'PuPlayer.playresume 4
		playbgaudio
		playclear pAudio
		PuPlayer.SetLoop 7,0
		hsEnteredName = hsEnteredDigits(1) & hsEnteredDigits(2) & hsEnteredDigits(3)
'		HighScoreName(4) = hsEnteredName
'		checkorder
If tmpScore > HighScore(4) Then	
	HighScore(4) = tmpScore
	HighScoreName(4) = hsEnteredName
	checkorder
End If 
If tmpComboChampion > HighScoreComboChampion Then
	HighScoreComboChampion = tmpComboChampion
	HighScoreComboChampionName = hsEnteredName
End If 
If tmpKnightChampion > HighScoreKnightChampion Then
	HighScoreKnightChampion = tmpKnightChampion
	HighScoreKnightChampionName = hsEnteredName
End If 
If tmpBlackCastleChampion > HighScoreBlackCastleChampion Then
	HighScoreBlackCastleChampion = tmpBlackCastleChampion
	HighScoreBlackCastleChampionName = hsEnteredName
End If 
If tmpLoopChampion > HighScoreLoopChampion Then
	HighScoreLoopChampion = tmpLoopChampion
	HighScoreLoopChampionName = hsEnteredName
End If 
If tmpWarChampion > HighScoreWarChampion Then
	HighScoreWarChampion = tmpWarChampion
	HighScoreWarChampionName = hsEnteredName
End If 
If tmpBonusChampion > HighScoreBonusChampion Then
	HighScoreBonusChampion = tmpBonusChampion
	HighScoreBonusChampionName = hsEnteredName
End If 
If tmpTripleKnightsMBChampion > HighScoreTripleKnightsMBChampion Then
	HighScoreTripleKnightsMBChampion = tmpTripleKnightsMBChampion
	HighScoreTripleKnightsMBChampionName = hsEnteredName
End If 
If tmpCatapultMBChampion > HighScoreCatapultMBChampion Then
	HighScoreCatapultMBChampion = tmpCatapultMBChampion
	HighScoreCatapultMBChampionName = hsEnteredName
End If 
If tmpMoltenFireChampion > HighScoreMoltenFireChampion Then
	HighScoreMoltenFireChampion = tmpMoltenFireChampion
	HighScoreMoltenFireChampionName = hsEnteredName
End If 
If tmpDeepFreezeChampion > HighScoreDeepFreezeChampion Then
	HighScoreDeepFreezeChampion = tmpDeepFreezeChampion
	HighScoreDeepFreezeChampionName = hsEnteredName
End If 
If tmpMudBogChampion > HighScoreMudBogChampion Then
	HighScoreMudBogChampion = tmpMudBogChampion
	HighScoreMudBogChampionName = hsEnteredName
End If 
If tmpWickedCavernChampion > HighScoreWickedCavernChampion Then
	HighScoreWickedCavernChampion = tmpWickedCavernChampion
	HighScoreWickedCavernChampionName = hsEnteredName
End If 
If tmpBurningSandsChampion > HighScoreBurningSandsChampion Then
	HighScoreBurningSandsChampion = tmpBurningSandsChampion
	HighScoreBurningSandsChampionName = hsEnteredName
End If 
If tmpSuperChampion > HighScoreSuperChampion Then
	HighScoreSuperChampion = tmpSuperChampion
	HighScoreSuperChampionName = hsEnteredName
End If

		checkorder
		Savehs
		If NewRecord = True Then
			NewRecord = False
			BypassVideo = False
			EndOfMode.Enabled=False
			HideOverlay = False
			restoreOverlay.Interval=100
			restoreOverlay.Enabled=True
		End If
		osbtemp = hsEnteredName
		'	if PlayersPlayingGame = 1 Then
		'		playmedia "","videogo",pBackglass,"",14000,"",1,1  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		'	end if
		if osbkey="" Then
		Else
'		SubmitOSBScore
		end If
		If PlayerCheckCurrent > 1 Then							'Check If There is another Champion (Check player 4 to 1 )
			PlayerCheckCurrent = PlayerCheckCurrent - 1
			CheckHighScore PlayerCheckCurrent
		Else
'			EndOfBallComplete()
			EndOfGame
			PuPlayer.LabelSet pBackglass,"HighScore","",1,""
			PuPlayer.LabelSet pBackglass,"HighScore2","",1,""
			PuPlayer.LabelSet pBackglass,"HighScoreL1","",1,""
			PuPlayer.LabelSet pBackglass,"HighScoreL2"," ",1,""
			PuPlayer.LabelSet pBackglass,"HighScoreL3"," ",1,""
			PuPlayer.LabelSet pBackglass,"HighScoreL4"," ",1,""
			hsbModeActive = False
		End If
	End Sub



'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  ATTRACT MODE
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' 

	Sub StartAttractMode()
		RedColorOnly = True
		pNote "Game Set","Start Anytime"
'		pupDMDDisplay "-","Game Set^Start Anytime",dmdnote,3,0,10
		cineon =0
		if bBallSaverActive = True then exit sub
		'PuPlayer.playpause 4
		playclear pMusic
		puPlayer.LabelSet pBackglass,"titleimg","",1,"{'mt':2,'color':111111, 'width': 0, 'height': 0, 'yalign': 0}"
		DOF 323, DOFOn   'DOF MX - Attract Mode ON
		bAttractMode = True
		StartLightSeq
		'ShowTableInfo
		DMDintroloop
		

		dim xx
		For each xx in GI:xx.State = 1:  Next '(plastics lights)



		'StartRainbow alights
		StartRainbow GI
		'StartRainbow aGIlights
		DMDattract.Enabled = 1
		intromover.enabled = true
		'PuPlayer.playlistplayex pMusic,songs,"",sndtrkvol,1
	End Sub

	Sub StopAttractMode()
		RedColorOnly = False
		pDMDStartGame
		DOF 323, DOFOff   'DOF MX - Attract Mode Off
		bAttractMode = False
		LightSeqAttract.StopPlay
		LightSeq002.StopPlay
''		LightSeqAttract2.StopPlay
		StopRainbow alights
'		StopRainbow2 GI
		ResetAllLightsColor
		DMDattract.Enabled = 0
		intromover.enabled = false
		
	'StopSong
	End Sub






'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  LIGHTING / RAINBOW LIGHTS
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' 


	'********************************************************************************************
	' Only for VPX 10.2 and higher.
	' FlashForMs will blink light or a flasher for TotalPeriod(ms) at rate of BlinkPeriod(ms)
	' When TotalPeriod done, light or flasher will be set to FinalState value where
	' Final State values are:   0=Off, 1=On, 2=Return to previous State
	'********************************************************************************************

	Sub FlashForMs(MyLight, TotalPeriod, BlinkPeriod, FinalState) 'thanks gtxjoe for the first version

		If TypeName(MyLight) = "Light" Then

			If FinalState = 2 Then
				FinalState = MyLight.State 'Keep the current light state
			End If
			MyLight.BlinkInterval = BlinkPeriod
			MyLight.Duration 2, TotalPeriod, FinalState
		ElseIf TypeName(MyLight) = "Flasher" Then

			Dim steps

			' Store all blink information
			steps = Int(TotalPeriod / BlinkPeriod + .5) 'Number of ON/OFF steps to perform
			If FinalState = 2 Then                      'Keep the current flasher state
				FinalState = ABS(MyLight.Visible)
			End If
			MyLight.UserValue = steps * 10 + FinalState 'Store # of blinks, and final state

			' Start blink timer and create timer subroutine
			MyLight.TimerInterval = BlinkPeriod
			MyLight.TimerEnabled = 0
			MyLight.TimerEnabled = 1
			ExecuteGlobal "Sub " & MyLight.Name & "_Timer:" & "Dim tmp, steps, fstate:tmp=me.UserValue:fstate = tmp MOD 10:steps= tmp\10 -1:Me.Visible = steps MOD 2:me.UserValue = steps *10 + fstate:If Steps = 0 then Me.Visible = fstate:Me.TimerEnabled=0:End if:End Sub"
		End If
	End Sub

	'******************************************
	' Change light color - simulate color leds
	' changes the light color and state
	' 10 colors: red, orange, amber, yellow...
	'******************************************
	' in this table this colors are use to keep track of the progress during the acts and battles

	'colors

	Sub gicolor(n)
		dim a
	End Sub

	Sub SetLightColor(n, col, stat)
		Select Case col
			Case red
				n.color = RGB(18, 0, 0)
				n.colorfull = RGB(255, 0, 0)
			Case orange
				n.color = RGB(18, 3, 0)
				n.colorfull = RGB(255, 64, 0)
			Case amber
				n.color = RGB(193, 49, 0)
				n.colorfull = RGB(255, 153, 0)
			Case yellow
				n.color = RGB(18, 18, 0)
				n.colorfull = RGB(255, 255, 0)
			Case darkgreen
				n.color = RGB(0, 8, 0)
				n.colorfull = RGB(0, 64, 0)
			Case green
				n.color = RGB(0, 18, 0)
				n.colorfull = RGB(0, 255, 0)
			Case blue
				n.color = RGB(0, 18, 18)
				n.colorfull = RGB(0, 255, 255)
			Case darkblue
				n.color = RGB(0, 8, 8)
				n.colorfull = RGB(0, 64, 64)
			Case purple
				n.color = RGB(128, 0, 128)
				n.colorfull = RGB(255, 0, 255)
			Case white
				n.color = RGB(255, 252, 224)
				n.colorfull = RGB(193, 91, 0)
			Case white
				n.color = RGB(255, 252, 224)
				n.colorfull = RGB(193, 91, 0)
			Case base
				n.color = RGB(255, 197, 143)
				n.colorfull = RGB(255, 255, 236)
		End Select
		If stat <> -1 Then
			n.State = 0
			n.State = stat
		End If
	End Sub

	Sub ResetAllLightsColor ' Called at a new game
		SetLightColor l13, red, -1		
	End Sub

	Sub UpdateBonusColors
	End Sub

	'*************************
	' Rainbow Changing Lights
	'*************************

	Sub StartRainbow(n)
		set RainbowLights = n
		RGBStep = 0
		RGBFactor = 5
		rRed = 255
		rGreen = 0
		rBlue = 0
		RainbowTimer.Enabled = 1
	End Sub

	Sub StopRainbow(n)
		Dim obj
		RainbowTimer.Enabled = 0
		RainbowTimer.Enabled = 0
			For each obj in RainbowLights
				SetLightColor obj, "white", 0
			Next
	End Sub

	Sub RainbowTimer_Timer 'rainbow led light color changing
		Dim obj
	For each obj in GI
				obj.color = RGB(rRed \ 10, rGreen \ 10, rBlue \ 10)
				obj.colorfull = RGB(rRed, rGreen, rBlue)
			Next
	End Sub
	Sub RainbowTimer1_Timer 'rainbow led light color changing
		Dim obj
		Select Case RGBStep2
			Case 0 'Green
				rGreen2 = rGreen2 + RGBFactor2
				If rGreen2 > 255 then
					rGreen2 = 255
					RGBStep2 = 1
				End If
			Case 1 'Red
				rRed2 = rRed2 - RGBFactor2
				If rRed2 < 0 then
					rRed2 = 0
					RGBStep2 = 2
				End If
			Case 2 'Blue
				rBlue2 = rBlue2 + RGBFactor2
				If rBlue2 > 255 then
					rBlue2 = 255
					RGBStep2 = 3
				End If
			Case 3 'Green
				rGreen2 = rGreen2 - RGBFactor2
				If rGreen2 < 0 then
					rGreen2 = 0
					RGBStep2 = 4
				End If
			Case 4 'Red
				rRed2 = rRed2 + RGBFactor2
				If rRed2 > 255 then
					rRed2 = 255
					RGBStep2 = 5
				End If
			Case 5 'Blue
				rBlue2 = rBlue2 - RGBFactor2
				If rBlue2 < 0 then
					rBlue2 = 0
					RGBStep2 = 0
				End If
		End Select
			For each obj in RainbowLights2
				obj.color = RGB(rRed2 \ 10, rGreen2 \ 10, rBlue2 \ 10)
				obj.colorfull = RGB(rRed2, rGreen2, rBlue2)
			Next
	End Sub

Sub TableAnimation
	'SaveStateLight
	AfterSaveStateLight.Enabled=True
End Sub

sub AfterSaveStateLight_Timer()
	GiOff2
	AfterSaveStateLight.Enabled=False
	On Error Resume Next
'		Dim a
'		For each a in alights
'			a.State = 1
'		Next
		CheckLamp.Enabled=False
	If SequenceAnimation = "1" Then
		TableAnimation01.Interval=4000
		TableAnimation01.Enabled=True
		L163.State = 1
		LightSeq003.UpdateInterval = 50
		LightSeq003.Play SeqBlinking, , 10, 40
		LightSeq004.UpdateInterval = 80
		LightSeq004.Play SeqBlinking, , 10, 40
	End If

	If SequenceAnimation = "2" Then
		TableAnimation01.Interval=4000
		TableAnimation01.Enabled=True
		L163.State = 1
		LightSeq002.UpdateInterval = 5
		LightSeq002.Play SeqUpOn, , 5, 1
	End If
end Sub

Sub TableAnimation01_Timer()
	StopAttractMode
	CheckLamp.Interval=100
	CheckLamp.Enabled=True
'	GiOff2
	TableAnimation01.Enabled=False
	relighttable
	ActionWhenHitOneOfThreeWay
	GiOn
	L163.State = 0
End Sub

Sub StartLightSeq()
		On Error Resume Next
		Dim a
'************ Change Lamp in round in RED *****************
LightObjectColor(1).color = RGB(255, 0, 0) 
LightObjectColor(1).colorfull = RGB(255, 0, 0)
LightObjectColor(2).color = RGB(255, 0, 0)
LightObjectColor(2).colorfull = RGB(255, 0, 0)
LightObjectColor(3).color = RGB(255, 0, 0)
LightObjectColor(3).colorfull = RGB(255, 0, 0)
LightObjectColor(4).color = RGB(255, 0, 0)
LightObjectColor(4).colorfull = RGB(255, 0, 0)
LightObjectColor(5).color = RGB(255, 0, 0)
LightObjectColor(5).colorfull = RGB(255, 0, 0)
LightObjectColor(6).color = RGB(255, 0, 0)
LightObjectColor(6).colorfull = RGB(255, 0, 0)
LightObjectColor(7).color = RGB(255, 0, 0)
LightObjectColor(7).colorfull = RGB(255, 0, 0)
LightObjectColor(8).color = RGB(255, 0, 0)
LightObjectColor(8).colorfull = RGB(255, 0, 0)
LightObjectColor(9).color = RGB(255, 0, 0)
LightObjectColor(9).colorfull = RGB(255, 0, 0)
LightObjectColor(10).color = RGB(255, 0, 0)
LightObjectColor(10).colorfull = RGB(255, 0, 0)
LightObjectColor(11).color = RGB(255, 0, 0)
LightObjectColor(11).colorfull = RGB(255, 0, 0)
LightObjectColor(12).color = RGB(255, 0, 0)
LightObjectColor(12).colorfull = RGB(255, 0, 0) 

		For each a in alights
			a.State = 1
		Next
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqUpOn, 50, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqDownOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqCircleOutOn, 15, 2
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqUpOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqDownOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqUpOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqDownOn, 25, 1
		LightSeqAttract.UpdateInterval = 10
		LightSeqAttract.Play SeqCircleOutOn, 15, 3
		LightSeqAttract.UpdateInterval = 5
		LightSeqAttract.Play SeqRightOn, 50, 1
		LightSeqAttract.UpdateInterval = 5
		LightSeqAttract.Play SeqLeftOn, 50, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqRightOn, 50, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqLeftOn, 50, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqRightOn, 40, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqLeftOn, 40, 1
		LightSeqAttract.UpdateInterval = 10
		LightSeqAttract.Play SeqRightOn, 30, 1
		LightSeqAttract.UpdateInterval = 10
		LightSeqAttract.Play SeqLeftOn, 30, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqRightOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqLeftOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqRightOn, 15, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqLeftOn, 15, 1
		LightSeqAttract.UpdateInterval = 10
		LightSeqAttract.Play SeqCircleOutOn, 15, 3
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqLeftOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqRightOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqLeftOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqUpOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqDownOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqUpOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqDownOn, 25, 1
		LightSeqAttract.UpdateInterval = 5
		LightSeqAttract.Play SeqStripe1VertOn, 50, 2
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqCircleOutOn, 15, 2
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqStripe1VertOn, 50, 3
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqLeftOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqRightOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqLeftOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqUpOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqDownOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqCircleOutOn, 15, 2
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqStripe2VertOn, 50, 3
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqLeftOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqRightOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqLeftOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqUpOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqDownOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqUpOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqDownOn, 25, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqStripe1VertOn, 25, 3
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqStripe2VertOn, 25, 3
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqUpOn, 15, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqDownOn, 15, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqUpOn, 15, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqDownOn, 15, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqUpOn, 15, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqDownOn, 15, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqRightOn, 15, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqLeftOn, 15, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqRightOn, 15, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqLeftOn, 15, 1

	End Sub

	Sub LightSeqAttract_PlayDone()
		StartLightSeq()
	End Sub

	Sub LightSeqTilt_PlayDone()
'		LightSeqTilt.Play SeqAllOff
	End Sub

	Sub LightSeqSkillshot_PlayDone()
		LightSeqSkillshot.Play SeqAllOff
	End Sub

	'**********************
	'     GI effects
	' independent routine
	' it turns on the gi
	' when there is a ball
	' in play
	'**********************

	Sub ChangeGi(col) 'changes the gi color
		Dim bulb
		For each bulb in GI
			SetLightColor bulb, col, -1
		Next
	End Sub

	Sub GIUpdateTimer_Timer
		Dim tmp, obj
		tmp = Getballs
		If UBound(tmp) <> OldGiState Then
			OldGiState = Ubound(tmp)
			If UBound(tmp) = 3 Then 'we have 4 captive balls on the table (-1 means no balls, 0 is the first ball, 1 is the second..)
				'GiOff               ' turn off the gi if no active balls on the table, we could also have used the variable ballsonplayfield.
			Else
				'Gion
			End If
		End If
	End Sub

	sub giflashing
		Dim bulb
		For each bulb in GI
			SetLightColor bulb, base, -1
			bulb.State = 1
			bulb.intensity = gibase
		Next
	end Sub

	Sub GiOn
'		L270.State=1
'		L271.State=1
'		L272.State=1
		gistatus = 1
'		GI_On_DOF_Undercab
		Dim bulb
		For each bulb in GI
'			SetLightColor bulb, base, -1
			bulb.State = 1
'			bulb.intensity = gibase
		Next
StartRainbow GI 
	End Sub

	Sub GiOff
		gistatus = 0
'		GI_Off_DOF_Undercab
		Dim bulb
		For each bulb in GI
			bulb.State = 0
		Next
'		spotsoff
	End Sub

	Sub GiOff2
		gistatus = 0
'		GI_Off_DOF_Undercab
		Dim bulb
		For each bulb in GI2
			bulb.State = 0
		Next
'		spotsoff
	End Sub

'	sub spotson
'		spotstatus = 1
''		spotleft.opacity = 300
''		spotright.opacity = 300
'	end Sub

'	sub spotsoff
'		spotstatus = 0
''		spotleft.opacity = 0
''		spotright.opacity = 0
'	end Sub


	' GI & light sequence effects


	Sub GiEffect(n)
		Select Case n
			Case 0 'all off
				'LightSeqGi.Play SeqAlloff
			Case 1 'all blink
				'LightSeqGi.UpdateInterval = 4
				'LightSeqGi.Play SeqBlinking, , 5, 100
			Case 2 'random
				'LightSeqGi.UpdateInterval = 10
				'LightSeqGi.Play SeqRandom, 5, , 1000
			Case 3 'upon
				'LightSeqGi.UpdateInterval = 4
				'LightSeqGi.Play SeqUpOn, 5, 1
			Case 4 ' left-right-left
				'LightSeqGi.UpdateInterval = 5
				'LightSeqGi.Play SeqLeftOn, 10, 1
				'LightSeqGi.UpdateInterval = 5
				'LightSeqGi.Play SeqRightOn, 10, 1
		End Select
	End Sub

	Sub LightEffect(n)
		Select Case n
			Case 0 ' all off
				'LightSeqInserts.Play SeqAlloff
			Case 1 'all blink
				'LightSeqInserts.UpdateInterval = 4
				'LightSeqInserts.Play SeqBlinking, , 5, 100
			Case 2 'random
				'LightSeqInserts.UpdateInterval = 10
				'LightSeqInserts.Play SeqRandom, 5, , 1000
			Case 3 'upon
				'LightSeqInserts.UpdateInterval = 4
				'LightSeqInserts.Play SeqUpOn, 10, 1
			Case 4 ' left-right-left
				'LightSeqInserts.UpdateInterval = 5
				'LightSeqInserts.Play SeqLeftOn, 10, 1
				'LightSeqInserts.UpdateInterval = 5
				'LightSeqInserts.Play SeqRightOn, 10, 1
			Case 5 'random
				'LightSeqbumper.UpdateInterval = 4
				'LightSeqbumper.Play SeqBlinking, , 5, 10
			Case 6 'random
				'LightSeqRSling.UpdateInterval = 4
				'LightSeqRSling.Play SeqBlinking, , 5, 6
			Case 7 'random
				'LightSeqLSling.UpdateInterval = 4
				'LightSeqLSling.Play SeqBlinking, , 5, 6
			Case 8 'random
				'LightSeqBack.UpdateInterval = 4
				'LightSeqBack.Play SeqBlinking, , 5, 6
			Case 12 'random
				'LightSeqlr.UpdateInterval = 4
				'LightSeqlr.Play SeqBlinking, , 5, 10
		End Select
	End Sub

	' Flasher Effects using lights

	Sub FlashEffect(n)
		Select case n
			Case 0 ' all off
				LightSeqFlasher.Play SeqAlloff
			Case 1 'all blink
				LightSeqFlasher.UpdateInterval = 4
				LightSeqFlasher.Play SeqBlinking, , 5, 100
			Case 2 'random
				LightSeqFlasher.UpdateInterval = 10
				LightSeqFlasher.Play SeqRandom, 5, , 1000
			Case 3 'upon
				LightSeqFlasher.UpdateInterval = 4
				LightSeqFlasher.Play SeqUpOn, 10, 1
			Case 4 ' left-right-left
				LightSeqFlasher.UpdateInterval = 5
				LightSeqFlasher.Play SeqLeftOn, 10, 1
				LightSeqFlasher.UpdateInterval = 5
				LightSeqFlasher.Play SeqRightOn, 10, 1
			Case 5 ' top flashers blink fast
		End Select
	End Sub

 '***************************************************
'     JP's VP10 Flashers v2 for original tables
'       Based on PD's Fading Light System
' SetFlash 0 is Off, 1 is On, 2 is blinking
'***************************************************

InitFlashers()
 
Sub FlashTimer_Timer()
    Flash 1, Flasher1
    Flash 2, Flasher2
    Flash 6, Flasher3
    Flash 7, Flasher4
    Flash 8, Flasher5
End Sub

Sub InitFlashers()
    Dim x
    For x = 0 to 100
        FlashState(x) = 0        ' current state: 0 off, 1 on, 2 blinking
        FlashSpeedUp(x) = 0.5    ' faster speed when turning on the flasher
        FlashSpeedDown(x) = 0.25 ' slower speed when turning off the flasher
        FlashMax(x) = 1          ' the maximum value when on, usually 1
        FlashMin(x) = 0          ' the minimum value when off, usually 0
        FlashLevel(x) = 0        ' the intensity of the flashers, usually from 0 to 1
        FlashRepeat(x) = 20      ' the default number of blinks in the state 2
    Next
    ' Fast flashers - the dome flashers
    For x = 9 to 12
        FlashSpeedUp(x) = 1     'instant on
        FlashSpeedDown(x) = 0.5 ' only 2 step
    Next
    FlashTimer.Interval = 20
    FlashTimer.Enabled = True
End Sub

Sub AllFlashersOff
    Dim x
    For x = 0 to 100
        SetFlash x, 0
    Next
End Sub

Sub SetFlash(n, value)
    If value <> FlashState(n) Then
        FlashState(n) = value
        FadingLevel(n) = value
    End If
End Sub

Sub Flash(n, object)
    Select Case FadingLevel(n)
        Case 0 'off
            FlashLevel(n) = FlashLevel(n) - FlashSpeedDown(n)
            If FlashLevel(n) < FlashMin(n) Then
                FlashLevel(n) = FlashMin(n)
                FadingLevel(n) = -1 'stopped
            End if
            Object.IntensityScale = FlashLevel(n)
        Case 1 ' on
            FlashLevel(n) = FlashLevel(n) + FlashSpeedUp(n)
            If FlashLevel(n) > FlashMax(n) Then
                FlashLevel(n) = FlashMax(n)
                FadingLevel(n) = -1 'stopped
            End if
            Object.IntensityScale = FlashLevel(n)
        Case 2 'blink -turn on flasher
            FlashLevel(n) = FlashLevel(n) + FlashSpeedUp(n)
            If FlashLevel(n) > FlashMax(n) Then
                FlashLevel(n) = FlashMax(n)
                FadingLevel(n) = 3
            End if
            Object.IntensityScale = FlashLevel(n)
        Case 3 'blink -turn off flasher
            FlashLevel(n) = FlashLevel(n) - FlashSpeedDown(n)
            If FlashLevel(n) < FlashMin(n) Then
                FlashLevel(n) = FlashMin(n)
                FlashRepeat(n) = FlashRepeat(n) - 1
                If FlashRepeat(n) = 0 Then
                    FlashRepeat(n) = 20 'reset for next time
                    FlashState(n) = 0
                    FadingLevel(n) = -1
                Else
                    FadingLevel(n) = 2
                End If
            End if
            Object.IntensityScale = FlashLevel(n)
    End Select
End Sub

Sub Flashm(n, object) 'multiple flashers, it just sets the flashlevel
    Object.IntensityScale = FlashLevel(n)
End Sub



'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'   SCORING FUNCTIONS
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
Sub AddScore(points)
	If Gamerover_flag = False Then 
		If Score(CurrentPlayer) => ReplayAt And ReplayPlayer(CurrentPlayer) = False Then
			ReplayPlayer(CurrentPlayer) = True
			VideoReplay
			AwardSpecial
		End If
		If(Tilted = False) Then
			' add the points to the current players score variable	
			dim dubpnts
			If RetroMode = 1 or RetroMode = 3 Then
				dubpnts = points * BallsOnPlayfield
				Score(CurrentPlayer) = Score(CurrentPlayer) + dubpnts
			Else	
				if awamor = 1 Then
					dubpnts = points * 2
					Score(CurrentPlayer) = Score(CurrentPlayer) + dubpnts
				else	
					Score(CurrentPlayer) = Score(CurrentPlayer) + points
				end if
			End If
		End If
	End if
End Sub

'	sub scoretimer_timer
'			pUpdateScores
'	end Sub

	Sub AddPoints(points)
		If(Tilted = False) Then
			' add the points to the current players score variable
			Select Case points
			Case 1
				Score(CurrentPlayer) = Score(CurrentPlayer) + 1000
			Case 2
				Score(CurrentPlayer) = Score(CurrentPlayer) + 10000
			Case 3
				Score(CurrentPlayer) = Score(CurrentPlayer) + 50000
			Case 4
				Score(CurrentPlayer) = Score(CurrentPlayer) + 100000
			Case 5
				Score(CurrentPlayer) = Score(CurrentPlayer) + 200000
			Case 6
				Score(CurrentPlayer) = Score(CurrentPlayer) + 500000
			Case 7
				Score(CurrentPlayer) = Score(CurrentPlayer) + 1000000
			Case 8
				Score(CurrentPlayer) = Score(CurrentPlayer) + 2000000
			Case 9
				Score(CurrentPlayer) = Score(CurrentPlayer) + 5000000
			Case 10
				Score(CurrentPlayer) = Score(CurrentPlayer) + 10000000
			End Select
		End if
	End Sub

	Sub AwardExtraBall()
		If NOT bExtraBallWonThisBall Then
'			t1.State = 1
'			flashflash.Enabled = True
'			LightSeqFlasher.UpdateInterval = 150
'			LightSeqFlasher.Play SeqRandom, 10, , 10000
			ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
			bExtraBallWonThisBall = True
			'extraballready(CurrentPlayer) = 0
			If bMultiBallMode = false Then
			'pNote "Extra","Ball!"
			'PuPlayer.playlistplayex pBackglass,"videoextraball","extraballsm.mov",100,1
			'PuPlayer.playlistplayex pCallouts,"audiocallouts","extra ball.mp3",vovol,1
			'chilloutthemusic
			End If
		Else
'		AddScore 4000000
		END If
	End Sub

	Sub AwardSpecial()
		Credits = Credits + 1
'		SaveCredits							'In comment because, in game the image block ~0,100m second
'		Savehs								'In comment because, in game the image block ~1,500m second
'		DOF 140, DOFOn
		'PlaySound SoundFXDOF("knocker",136,DOFPulse,DOFKnocker)		
		playmedia "Sound-0x08E4.mp3","Audiocomment",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		'DOF 115, DOFPulse 'Strobe
		GiEffect 1
		LightEffect 1
'		DOF 700, DOFPulse   'DOF MX - Replay
	End Sub

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'   BALL FUNCTIONS & DRAINS
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  


	Function Balls
		Dim tmp
		tmp = bpgcurrent - BallsRemaining(CurrentPlayer) + 1
		If tmp> bpgcurrent Then
			Balls = bpgcurrent
		Else
			Balls = tmp
		End If
	End Function


	Sub FirstBall_timer()
		MusicCheck 22
		FirstBall.Enabled=False
		ResetForNewPlayerBall()
		DrainTimer.Interval=2000
		DrainTimer.Enabled=True
		bballfirstball = 1
		'PuPlayer.playlistplayex pBackglass,"videoscenes","triwizardcup.mp4",0,1
		'playmedia "triwizardcup.mp4","videoscenes",pBackglass,"",3000,"",0,1  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	End Sub

	Sub ResetForNewPlayerBall()
		OrbitStatus.Enabled = True
		OrbitFlag = False
		playclear pBackglass
		BallControlTimer.Enabled = False
		DisableTable False
		ExtraBallsIsLit = False
		SuperTargets = False
		SuperSlings = False
		SuperLanes = False
		SuperLanes = False
		SuperPops = False
		SuperOrbits = False
		SuperSpinner = False
		SuperIsLit = False
		TimeSuperMode.Enabled = False
		'/* UpdateSuperOverlay
		'/*
		restoreOverlay.Interval=100
		restoreOverlay.Enabled=True
		'/*
		horloge.Interval=1000
		LastChanceStart = False
		LastChanceFlag = False
		LastChanceSuccess = False
		LastChanceCatapultMode = False 
		LastChanceChronoStart(CurrentPlayer) = False
		LastChanceButtonPush(CurrentPlayer) = False
		ChangeSuperMode = 0
		WarCount = 1
		AddScoreMudBog = 0
		AddScoreMudBogSliced = 0
		AddScoreMudBogSealed = 750000
		AddScoreMoltenFire = 1000000
		AddScoreDeepFreezeFrozen = 250000
		AddScoreDeepFreezeAwarded = 1000000
		AddScoreSandWorm = 1000000
		AddScoreWickedCavern = 1000000
		AddScoreTripleKnightChallengeKnightHited = 1000000
		AddScoreTripleKnightChallengeGoldRoom = 1000000
		AddScoreTripleKnightChallenge = 0
		AddScoreWarHurryUp = 1000000



		AddScoreMudBogBonusLoopSliced = 0
		AddScoreMudBogBonusLoopSealed = 0
		AddScoreMoltenFireBonusLoop = 0
		AddScoreSandWormLoop = 0
		AddScoreWickedCavernLoop = 0
		AddScoreDeepFreezeAwardedBonusLoop = 0
		AddScoreDeepFreezeFrozenBonusLoop = 0
		AddScoreBlackCastleLoop = 0

		OldMudBogDefeated = 0
		OldMoltenFireDefeated = 0
		OldBurningSandsDefeated = 0
		OldWickedCavernDefeated = 0
		OldDeepFreezeDefeated = 0
		OldBlackCastleDefeated = 0

		Catapult_jackpot_collected = 0
		VideoSetBackground
		WarHurryUp51Touch=0
		WarHurryUp56Touch=0
		WarHurryUp58Touch=0
		AddScoreLoopBonus = 500000
		AddScoreSW82 = 1000000
		AddscoreCatapultBonus = 400000
'		If WarHurryFlag = True Then						'WarHurry flag is set False but not here  --> See Drain
'			WarHurryFlag = False
'			EndOfMode.Interval=100
'			EndOfMode.Enabled=True
'		End If
		WarHurryPhase = 0
		BurningSandsHit = 0
		AddscoreCatapult = 0

		NewRecord = False
		RightGate.Collidable=False
		RightGateTW.Collidable=True
		If SW41Dropped = True Then
			SW41.IsDropped=False
			SW41Dropped = False
			playsound "fx2_droptargetreset"
		Else
			' Comment
		End If
		'PuPlayer.playlistplayex pCallouts,"audiocallouts","ballready.mp3",vovol,1
		playmedia "ballready.mp3","audiocallouts",pCallouts,"",1000,"",1,1  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		'PuPlayer.playresume 4
		'playbgaudio
		If PlayersPlayingGame > 1 Then
			If CurrentPlayer = 1 Then
				'playmedia "player1.mp4","videoscenes",pBackglass,"",5000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				SpeakTime = 1257
				LightEyesBK
				playmedia "Sound-0x0215.mp3","Audioknight",pCallouts,"",1000,"",1,1  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				playmedia "","videodrains",pBackglass,"",6500,"",1,1
			Elseif currentplayer = 2 Then
				'playmedia "player2.mp4","videoscenes",pBackglass,"",5000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				SpeakTime = 1510
				LightEyesBK
				playmedia "Sound-0x031B.mp3","Audioknight",pCallouts,"",1000,"",1,1  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			Elseif currentplayer = 3 Then
				'playmedia "player3.mp4","videoscenes",pBackglass,"",5000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				SpeakTime = 1617
				LightEyesBK
				playmedia "Sound-0x017A.mp3","Audioknight",pCallouts,"",1000,"",1,1  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			Elseif currentplayer = 4 Then
				'playmedia "player4.mp4","videoscenes",pBackglass,"",5000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				SpeakTime = 1503
				LightEyesBK
				playmedia "Sound-0x0353.mp3","Audioknight",pCallouts,"",1000,"",1,1  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			End If
		Else
			playmedia "player1.mp4","videoscenes",pBackglass,"",5000,"",1,1  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			playmedia "player1.mp3","audiocallouts",pCallouts,"",1000,"",1,1  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		End If
		AddScore 0
		BonusPoints(CurrentPlayer) = 0
		' 	SCORE rules
		AddScoreLoopBonus = 500000
		'	End SCORE rules	
		bBonusHeld = False
		bExtraBallWonThisBall = False
		ResetNewBallLights()
		'ResetNewBallVariables
		bBallSaverReady = True
		bSkillShotReady = True
		relighttable
'		wandclose
		'playmedia "",songs,pMusic,"",0,"",1,4
		playbgaudio
		'vpmtimer.addtimer 300, "playbgaudio '"
		'PuPlayer.SetLoop 4,1
        pDMDStartBall 'pupdmd
'/**/			
'			timer004.Interval=100						--> This line addscore 10,000 at the begining of each new ball 
'			timer004.Enabled = True						--> This line addscore 10,000 at the begining of each new ball  
			KnightChallengeTimer.Enabled = False
			StartMission = 0
'/**/
	End Sub

	sub destroycalc
		BallsOnPlayfield = BallsOnPlayfield - 1
		If merlock1full = 1 Then
			BallsOnPlayfield = BallsOnPlayfield - 1
		end If
		If merlock2full = 1 Then
			BallsOnPlayfield = BallsOnPlayfield - 1
		end If
		If merlock3full = 1 Then
			BallsOnPlayfield = BallsOnPlayfield - 1
		end If
	end Sub

	Sub DrainTimer_Timer()
		DrainTimer.Enabled=False
		relighttable
		If ShootAgain = True Then
			ShootAgain = False
			VideoExtraBallShootAgain
			ShootAgainDelay.Enabled = True
		Else
			CreateNewBall()
		End If
	end Sub

	Sub ShootAgainDelay_Timer()
		ShootAgainDelay.Enabled = False
		CreateNewBall()
	End Sub

	Sub CreateNewBall()
		currentqueue = " "
		Kicker1.CreateBall
		'BallsOnPlayfield = BallsOnPlayfield + 1
'		PlaySoundAt SoundFXDOF("ballrelease", 114, DOFPulse, DOFContactors), BallRelease
		Kicker1.Kick 300, 30, 1.56
		'CastleVUK2.kick 300, 30, 1.56
		playsound SoundFXDOF("ballrelease", 110, DOFPulse, DOFContactors)
		DOF 205, DOFPulse 'Ball ready to shoot
		if bBallSaverActive = False Then
			if bballfirstball = 0  and bBallSaverReady = false Then
				EnableBallSaver 5
			end if
		end If
		If BallsOnPlayfield > 1 Then
			'bMultiBallMode = True
			bAutoPlunger = True
		End If

		if bballfirstball = 0 Then
			'bMultiBallMode = True
			bAutoPlunger = True
		End if 
		
'		If BallSaverTimerExpired.Enabled = False Then
'			bBallSaverActive = False
'		End If 
	End Sub

	sub fireit
			'PlungerIM.Strength = 45
			'PlungerIM.Strength = Plunger.MechStrength
			Plunger.AutoPlunger = true
			Plunger.Pullback
			Plunger.Fire
			'PlungerIM.AutoFire
			bAutoPlunger = False
			Plunger.AutoPlunger = false
	end Sub

	Sub AddMultiball(nballs)
		mBalls2Eject = mBalls2Eject + nballs
		CreateMultiballTimer.Enabled = True
	End Sub

	Sub CreateMultiballTimer_Timer()
		If bBallInPlungerLane Then
			Exit Sub
		Else
			If BallsOnPlayfield <MaxMultiballs Then
				DrainTimer.Interval=500
				DrainTimer.Enabled=True
				mBalls2Eject = mBalls2Eject -1
				If mBalls2Eject = 0 Then 
					Me.Enabled = False
				End If
			Else 
				mBalls2Eject = 0
				Me.Enabled = False
			End If
		End If
	End Sub

Sub EndOfBall()
	If LastChanceSuccess = False Then
		bGameInPlay = True
	End If
	playclear pAudio
	bMultiBallMode = False
	bOnTheFirstBall = False
	Tilted = False
	Tilt = 0
	DisableTable False
	tilttableclear.enabled = False
	tilttime = 0
	If(ExtraBallsAwards(CurrentPlayer) <> 0) Then
		ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) - 1
		DrainTimer.Interval=7500
		DrainTimer.Enabled=True
		bballfirstball = 1
		ResetForNewPlayerBall
		If RetroMode = 1 or RetroMode = 3 Then
			'comment
		Else
			ShootAgain = True
		End If
	Else
		BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer) - 1
'		If(BallsRemaining(CurrentPlayer) <= 0) Then
'			CheckHighScore()
'			7067
'		Else
'		EndOfBallComplete()
		EndOfBallComplete.Enabled = True
'		End If
	End If
End Sub
	
Sub EndOfBallComplete_Timer()
	EndOfBallComplete.Enabled = False
	'ResetNewBallVariables
	Dim NextPlayer
	If(PlayersPlayingGame> 1) Then
		TurnOffRoundLights					'before update player
'		AddTimeForMission(CurrentPlayer)=0
		NextPlayer = CurrentPlayer + 1
		If(NextPlayer> PlayersPlayingGame) Then
			NextPlayer = 1
		End If
		If CurrentMissionFlag(NextPlayer) = 0 Then
			MissionRandomTimer.Interval=500
			MissionRandomTimer.Enabled=True
		End If
	Else
		NextPlayer = CurrentPlayer
'		If CurrentMissionFlag(NextPlayer) = 0 Then				'Don't change the mission when there is only player 1 loose the Ball 
'			MissionRandomTimer.Interval=500
'			MissionRandomTimer.Enabled=True
'		End If
	End If
	If((BallsRemaining(CurrentPlayer) <= 0) AND(BallsRemaining(NextPlayer) <= 0) ) Then
		CheckHighScore CurrentPlayer
		InProgress= True
	Else
		CurrentPlayer = NextPlayer
		CheckLamp.Enabled=True
		AddScore 0
		ResetForNewPlayerBall()
		DrainTimer.Interval=3000
		DrainTimer.Enabled=True
		bballfirstball = 1
		DisableTable False
		bGameInPlay = True
	End If
End Sub

'******************************************************************************************************
'*
'*
'*							DRAIN
'*
'*
'*******************************************************************************************************
'* Base Bonus
'* Super Features
'* Combos
'* Loops
'* Total
'*
'*
'* 

Sub Drain_Hit()
	BallRemainingPrevision = BallsRemaining(CurrentPlayer) - 1
	BallsOnPlayfieldPrevision = BallsOnPlayfield - 1
	DrainerCheck.interval = 50
	If BallRemainingPrevision = 0 Then
		DrainerCheck.interval = 1000
	End If
	DrainerCheck.Enabled = True
	BallControlTimer.Enabled = False
End Sub

Sub DisplayScore_Timer()
	DisplayScore.Enabled = False
	Video_DisplayScore02
End Sub

Sub CheckIfVideoIsPlayed_Timer()
	CheckIfVideoIsPlayed.Enabled = False
	If VideoIsPlayed = True Then
		CheckIfVideoIsPlayed.Enabled = True
	Else
		playclear pBackglass
		DisplayScore.interval = 1000
		DisplayScore.Enabled = True
	End If
End Sub

Sub DrainerCheck_Timer()
	BallControlTimer.Enabled = False
	DrainerCheck.Enabled = False
	EndOfBallComplete.Interval=7000
	BallsOnPlayfield = BallsOnPlayfield - 1
	If BallsOnPlayfield < 0 Then BallsOnPlayfield = 0 End If
	rememberlights
	Drain.DestroyBall
'	BallRemainingPrevision = BallsRemaining(CurrentPlayer) - 1
	BonusMultiplier(CurrentPlayer) = WarCount
	SuperModeDelay = 1
	StopSuperMode
	If Tilted = True Then												'***************** TILTED ****************************
		EndOfBallComplete.Interval=100
		If BallsOnPlayfield <= 0 Then 
			'Sub NextBall  'do not display Score Bonus
			'***************** Nex Line has been on the previous version
			DrainHited.Enabled = False								'If The table is Tilted and after x sec no ball drained -->
			UnblockDrainHited.Enabled = False						'If The table is Tilted and after x sec no ball drained -->
			'***************** Nex Line has been on the previous version - sub Drainer_timer
			BallsOnPlayfield = 0
			bMultiBallMode = False
			StopEndOfBallMode
			drainvids
			MusicCheck 1
			CatapultModeFlag = False
			StopBouleAngle = 90
			StopRotating.interval=100
			StopRotating.Enabled=True			
			Catapult_Super_Jackpot_Multi = 1
			Knight_challenge_flag = False
			RetroMode = 0
'			AddTimeForMission(CurrentPlayer) = 0 ----> Verifier quand on est sur une mission et le mode retro..(mettre à zero ou pas?)
			If BlackKnightRetro(CurrentPlayer) = 1 Then
				BlackKnightRetro(CurrentPlayer) = 2			'End Of Mode BK
				RetroMode = 0
				AddTimeForMission(CurrentPlayer) = 0
				horloge.Enabled=False
				CurrentMissionFlag(CurrentPlayer) = 0
			Elseif BlackKnightRetro(CurrentPlayer) = 3 Then
				AwardExtraBall
				StopRansom = False
				BlackKnightRetro(CurrentPlayer) = 0
				ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
				RetroMode = 0
				AddTimeForMission(CurrentPlayer) = 0
			End If
		Else
			'none wait ball on playfield = 0
			BallsOnPlayfield = 0
		End If
	ElseIf bBallSaverActive = True Then										'***************** Ball Saver is active **************
		Video_SavedBall
		playsound SoundFXDOF("ballrelease", 110, DOFPulse, DOFContactors)
		AutoFireTimer.Enabled=true	'Send new ball, because BBSaverIsActive
		If bBallSaverActiveL13 = True Then
			BallSaveAvailable(CurrentPlayer) = True
			bBallSaverActiveL13 = False
		End If
	ElseIf bBallSaverActiveL13 = True Then									'***************** Ball Saver Left outlane is active **************
		Video_SavedBall
		BallSaveAvailable(CurrentPlayer) = False
		bBallSaverActiveL13 = False
		AutoFireTimer.Enabled=true	'Send new ball, because BBSaverIsActive
'	ElseIf LastChanceFlag = True And CurrentMissionFlag(CurrentPlayer) = 3 Then	   		'***************** Last Chance is active *************					'Check if correct start, current and after...
	ElseIf LastChanceFlag = True And LastChanceDrainUncompleted = True Then	   		'***************** Last Chance is active *************					'Check if correct start, current and after...
'		ValueBox.text = "LastChance"
		If horloge.Enabled=True Then
			'None Kickball is always available
		ElseIf BallsOnPlayfield <= 0 Then
			If LastChanceSuccess = True Then
				DisableTable False
				LastChanceClearTable = False
'				LastChanceStart = False
				'Start Catapult Last chance
				'The ball is unlock after the video is played
				VideoCatapultLoopSetBackground.Interval = 100
				VideoCatapultLoopSetBackground.Enabled = True
				horloge.Interval=1000
			Else

				PuPlayer.playstop pCallouts
				playclear pMusic
				playclear pAudio
				EndOfBall
				restoreOverlay.Interval=100
				restoreOverlay.Enabled=True
'				LockerTimer.Enabled=True
			End If
			restoreOverlay.Interval=100
			restoreOverlay.Enabled=True
			puPlayer.LabelSet pBackglass,"LastChanceBallToLock","",1,""
			puPlayer.LabelSet pBackglass,"LastChanceTime","",1,""
			puPlayer.LabelSet pBackglass,"LastChanceJackpot","",1,""
			LastChanceDrainUncompleted = False
		Else
			'DisableTable True
			'LastChanceClearTable = True
			'End Mode 
			'Sub NextBall  'do not display Score Bonus
		End If
	ELseIf LastChanceButtonPush(CurrentPlayer) = True And LastChanceChronoStart(CurrentPlayer) = False And ExtraBallsAwards(CurrentPlayer) = 0 Then		'***************** Last Chance is enable *************
'		ValueBox.text = "LstBouton"
		mMagnetSave.MagnetOn = 0
		LastChanceFlag = True
'		VideoLastChanceIntro
		VideoLastChanceIntro.Enabled = True
		L142.State=2
		L143.State=2
		L270.State=0
		L271.State=0
		L272.State=0
		MusicCheck 777
	ElseIf Gamerover_flag = True Then											'***************** GAME IS OVER **********************	
		'None The Game is Over		
	ElseIf BlackKnightRetro(CurrentPlayer) = 1 Then							'***************** Mode Retro BK *********************
		If BallsOnPlayfield > 1 Then
			'None
		ElseIf BallsOnPlayfield = 1 Then
			BlackKnightRetro(CurrentPlayer) = 2			'End Of Mode BK
			RetroMode = 0
			AddTimeForMission(CurrentPlayer) = 0
			horloge.Enabled=False
			CurrentMissionFlag(CurrentPlayer) = 0
			HideOverlay = False
			restoreOverlay.Interval=100		'restore Overlay
			restoreOverlay.Enabled=True
			MusicCheck 86

		Else
			BlackKnightRetro(CurrentPlayer) = 2			'End Of Mode BK
			RetroMode = 0
			AddTimeForMission(CurrentPlayer) = 0
			horloge.Enabled=False
			CurrentMissionFlag(CurrentPlayer) = 0
			MusicCheck 1
			'End Mode 
			'Display Score Bonus
'NOT POSSIBLE
'			playclear pBackglass
'			DisplayScore.interval = 500
'			DisplayScore.Enabled = True
		End If
	ELseIf BlackKnightRetro(CurrentPlayer) = 3 Then							'***************** Mode Retro BK2000 *****************
		If AddTimeForMission(CurrentPlayer) > 0 Then	
			If AddTimeForMission(CurrentPlayer) > 5 Then
				AutoFireTimer.Enabled=true
			End If
		Else
			'sub RetroBK2000 Bonus Score
			If BallsOnPlayfield < 1 Then
				AwardExtraBall
				DisableTable False
				StopRansom = False
				BlackKnightRetro(CurrentPlayer) = 0
				ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
'				EndOfBall2
				EndOfBall
				RetroMode = 0
				restoreOverlay.Interval=100
				restoreOverlay.Enabled=True
				StopBouleAngle = 90						'In test
				StopRotating.interval=100				'In test
				StopRotating.Enabled=True				'In test
			End If
		End If
	ElseIf CatapultModeFlag = True Or Knight_challenge_flag = True Then		'***************** Mode Catapult & TKC ***************
		If BallsOnPlayfield > 1 Then
			'None
		ElseIf BallsOnPlayfield = 1 And LastChanceCatapultMode = True And BallInCatapult(CurrentPlayer) > 0 Then
			'None
		ElseIf BallsOnPlayfield = 0 And LastChanceCatapultMode = True And BallInCatapult(CurrentPlayer) > 0 Then
			LastChanceBallInCatapult.interval = 100
			LastChanceBallInCatapult.Enabled = True
			TimerBeforeBonusScore = 0
			If CatapultModeFlag = True Then
				CatapultModeFlag = False
				StopBouleAngle = 90
				StopRotating.interval=100
				StopRotating.Enabled=True
				Catapult_Super_Jackpot_Multi = 1
				ModeTotalCatapult.Enabled = True
				VideoIsPlayed = True
				CurrentMissionFlag(CurrentPlayer) = 0
'				AddTimeToDrainForDisplayModeTotal = 6750    '4750 + 100 + 2000
				TimerBeforeBonusScore = TimerBeforeBonusScore + 6750
			End If
			If Knight_challenge_flag = True Then
				If TimerBeforeBonusScore <> 0 Then	
					TimerBeforeBonusScore = TimerBeforeBonusScore + 4000
					'Display Mode Total Knight_challenge.interval = 5000
				Else
					TimerBeforeBonusScore = 100
					'Display Mode Total Knight_challenge.interval = 100				
				End If
				KnightRemaining = 3
				Knight_challenge_flag = False
				ModeTotalKnightChallenge.Enabled = True
				VideoIsPlayed = True
				CurrentMissionFlag(CurrentPlayer) = 0
'				CheckIfVideoIsPlayed.interval = TimerBeforeBonusScore
'				CheckIfVideoIsPlayed.Enabled = True 
			End If
		ElseIf BallsOnPlayfield <= 1 Then
			TimerBeforeBonusScore = 0
			If CatapultModeFlag = True Then
				CatapultModeFlag = False
				StopBouleAngle = 90						'In test
				StopRotating.interval=100				'In test
				StopRotating.Enabled=True				'In test

				Catapult_Super_Jackpot_Multi = 1
				ModeTotalCatapult.Enabled = True
				VideoIsPlayed = True
				CurrentMissionFlag(CurrentPlayer) = 0
'				AddTimeToDrainForDisplayModeTotal = 6750    '4750 + 100 + 2000
				TimerBeforeBonusScore = TimerBeforeBonusScore + 6750
			End If
			If Knight_challenge_flag = True Then
				If TimerBeforeBonusScore <> 0 Then	
					TimerBeforeBonusScore = TimerBeforeBonusScore + 4000
					'Display Mode Total Knight_challenge.interval = 5000
				Else
					TimerBeforeBonusScore = 100
					'Display Mode Total Knight_challenge.interval = 100				
				End If
				KnightRemaining = 3
				Knight_challenge_flag = False
				ModeTotalKnightChallenge.Enabled = True
				VideoIsPlayed = True
				CurrentMissionFlag(CurrentPlayer) = 0
'				CheckIfVideoIsPlayed.interval = TimerBeforeBonusScore
'				CheckIfVideoIsPlayed.Enabled = True 
			End If
			
		Else
		'Not possible
'			TimerBeforeBonusScore = 0
'			If CatapultModeFlag = True Then
'				'Display Mode Total CatapultModeFlag
'				TimerBeforeBonusScore = TimerBeforeBonusScore + 4750
'				CatapultModeFlag = False
'			End If
'			If Knight_challenge_flag = True Then
'				If TimerBeforeBonusScore <> 0 Then							'There is 2 Mode Total at display!!! First Catapult Mode Total and after TKC
'					'Display Mode Total Knight_challenge.interval = 5000
'					'Display Mode Total Knight_challenge_TIMER!!!! .Enabled = True
'				Else
'					'Display Mode Total Knight_challenge.interval = 100
'					'Display Mode Total Knight_challenge_TIMER!!!! .Enabled = True
'				End If			
'				TimerBeforeBonusScore = TimerBeforeBonusScore + 4000
'				Knight_challenge_flag = False
'			End If
'			If  BallsOnPlayfield <= 0 Then			 'Normaly this condition is not possible... because CatapultModeFlag or Knight_challenge_flag is False if BallsOnPlayfield = 1
'				DisplayScore.interval = 500
'				DisplayScore.Enabled = True
'			End If
		End If
	ElseIf CurrentMissionFlag(CurrentPlayer) = 1 Then										'***************** Mission is active *****************
		If BallsOnPlayfield > 0 Then
			' None
		Else 
			'End Mode 
			'Display Score Bonus
		'	playclear pBackglass
		'	DisplayScore.interval = 500
		'	DisplayScore.Enabled = True
			CheckIfVideoIsPlayed.Enabled = True
			drainvids
		End If
	Else																	'***************** the rest condition ****************
		CatapultModeFlag = False
		StopBouleAngle = 90						'In test
		StopRotating.interval=100				'In test
		StopRotating.Enabled=True				'In test
		If BallsOnPlayfield <= 0 Then
			bMultiBallMode = False
			'Display Score Bonus
'			DisplayScore.interval = 500
'			playclear pBackglass
'			DisplayScore.Enabled = True


			CheckIfVideoIsPlayed.Enabled = True					'Display the Bonus Score

			Catapult_Super_Jackpot_Multi = 1
			StopEndOfBallMode
			drainvids
			MusicCheck 1
		Else
			'Not Possible or Mode not finish correctly  2 balls on playfield
'			StopEndOfBallMode
'			drainvids
'			MusicCheck 1
		End If	
	End If
End Sub


sub drainvids
	playclear pMusic
	playclear pAudio
	EndOfBall
end sub

'********************************************************************************************************************************************
'********************************************************************************************************************************************
'********************************************************************************************************************************************
'********************************************************************************************************************************************
'********************************************************************************************************************************************
'FRAMEWORK BASE CODE END HERE
'********************************************************************************************************************************************
'********************************************************************************************************************************************
'********************************************************************************************************************************************
'********************************************************************************************************************************************
'********************************************************************************************************************************************

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  *ATTRACT MODE* 
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' 

introposition = 0

	Sub DMDintroloop
		DOF 300, DOFOn
'		PuPlayer.LabelSet pBackglass,"modetitle","",1,"{'mt':2,'color':16777215, 'size': 0, 'xpos': 80.7, 'xalign': 1, 'ypos': 72.6, 'yalign': 0}"
		introtime = 0
		clearhslabels
		clearosblabels
		introposition = introposition + 1
		Select Case introposition

		Case 1
			clearhslabels
			clearosblabels
			playclear pMusic
			'PuPlayer.playpause 4
			'playmedia "BlackKnight_AttractModeLoopingBG.mp4","Background looping",pBackglass,"",53000,"",1,10
			'DOF 504, DOFPulse
			pupevent 504
			playclear pMusic
'			Last Player --> Player 1			| 00

		Case 2
			'PuPlayer.playpause 4
			playclear pMusic
			clearhslabels
			clearosblabels
			playmedia "LogoStern.png","PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			
		Case 3
			clearosblabels
			playmedia "overlay_blank.png","PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			If PlayersPlayingGame = 4 Then
				playmedia "4PGameOver.png","PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				PuPlayer.LabelSet pBackglass,"GameOverPlayer1","Player 1",1,"{'mt':2,'color':28671, 'size': 6, 'xpos': 25, 'xalign': 1, 'ypos': 16, 'yalign': 1}"
				PuPlayer.LabelSet pBackglass,"ScoreGameOverPlayer1",FormatNumber(Score(1),0),1,"{'mt':2,'color':28671, 'size': 6, 'xpos': 25, 'xalign': 1, 'ypos': 24, 'yalign': 1}"
				PuPlayer.LabelSet pBackglass,"GameOverPlayer2","Player 2",1,"{'mt':2,'color':28671, 'size': 6, 'xpos': 75, 'xalign': 1, 'ypos': 16, 'yalign': 1}"
				PuPlayer.LabelSet pBackglass,"ScoreGameOverPlayer2",FormatNumber(Score(2),0),1,"{'mt':2,'color':28671, 'size': 6, 'xpos': 75, 'xalign': 1, 'ypos': 24, 'yalign': 1}"
				PuPlayer.LabelSet pBackglass,"GameOverPlayer3","Player 3",1,"{'mt':2,'color':28671, 'size': 6, 'xpos': 25, 'xalign': 1, 'ypos': 60, 'yalign': 1}"
				PuPlayer.LabelSet pBackglass,"ScoreGameOverPlayer3",FormatNumber(Score(3),0),1,"{'mt':2,'color':28671, 'size': 6, 'xpos': 25, 'xalign': 1, 'ypos': 68, 'yalign': 1}"
				PuPlayer.LabelSet pBackglass,"GameOverPlayer4","Player 4",1,"{'mt':2,'color':28671, 'size': 6, 'xpos': 75, 'xalign': 1, 'ypos': 60, 'yalign': 1}"
				PuPlayer.LabelSet pBackglass,"ScoreGameOverPlayer4",FormatNumber(Score(4),0),1,"{'mt':2,'color':28671, 'size': 6, 'xpos': 75, 'xalign': 1, 'ypos': 68, 'yalign': 1}"
			ElseIf PlayersPlayingGame = 3 Then
				playmedia "3PGameOver.png","PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				PuPlayer.LabelSet pBackglass,"GameOverPlayer1","Player 1",1,"{'mt':2,'color':28671, 'size': 6, 'xpos': 25, 'xalign': 1, 'ypos': 16, 'yalign': 1}"
				PuPlayer.LabelSet pBackglass,"ScoreGameOverPlayer1",FormatNumber(Score(1),0),1,"{'mt':2,'color':28671, 'size': 6, 'xpos': 25, 'xalign': 1, 'ypos': 24, 'yalign': 1}"
				PuPlayer.LabelSet pBackglass,"GameOverPlayer2","Player 2",1,"{'mt':2,'color':28671, 'size': 6, 'xpos': 75, 'xalign': 1, 'ypos': 16, 'yalign': 1}"
				PuPlayer.LabelSet pBackglass,"ScoreGameOverPlayer2",FormatNumber(Score(2),0),1,"{'mt':2,'color':28671, 'size': 6, 'xpos': 75, 'xalign': 1, 'ypos': 24, 'yalign': 1}"
				PuPlayer.LabelSet pBackglass,"GameOverPlayer3","Player 3",1,"{'mt':2,'color':28671, 'size': 6, 'xpos': 25, 'xalign': 1, 'ypos': 60, 'yalign': 1}"
				PuPlayer.LabelSet pBackglass,"ScoreGameOverPlayer3",FormatNumber(Score(3),0),1,"{'mt':2,'color':28671, 'size': 6, 'xpos': 25, 'xalign': 1, 'ypos': 68, 'yalign': 1}"
			ElseIf PlayersPlayingGame = 2 Then
				playmedia "2PGameOver.png","PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				PuPlayer.LabelSet pBackglass,"GameOverPlayer1","Player 1",1,"{'mt':2,'color':28671, 'size': 6, 'xpos': 25, 'xalign': 1, 'ypos': 16, 'yalign': 1}"
				PuPlayer.LabelSet pBackglass,"ScoreGameOverPlayer1",FormatNumber(Score(1),0),1,"{'mt':2,'color':28671, 'size': 6, 'xpos': 25, 'xalign': 1, 'ypos': 24, 'yalign': 1}"
				PuPlayer.LabelSet pBackglass,"GameOverPlayer2","Player 2",1,"{'mt':2,'color':28671, 'size': 6, 'xpos': 75, 'xalign': 1, 'ypos': 16, 'yalign': 1}"
				PuPlayer.LabelSet pBackglass,"ScoreGameOverPlayer2",FormatNumber(Score(2),0),1,"{'mt':2,'color':28671, 'size': 6, 'xpos': 75, 'xalign': 1, 'ypos': 24, 'yalign': 1}"
			Else
				playmedia "1PGameOver.png","PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				PuPlayer.LabelSet pBackglass,"GameOverPlayer1","Player 1",1,"{'mt':2,'color':28671, 'size': 6, 'xpos': 25, 'xalign': 1, 'ypos': 16, 'yalign': 1}"
				PuPlayer.LabelSet pBackglass,"ScoreGameOverPlayer1",FormatNumber(Score(1),0),1,"{'mt':2,'color':28671, 'size': 6, 'xpos': 25, 'xalign': 1, 'ypos': 24, 'yalign': 1}"
			End If
		Case 4
			clearhslabels
			clearosblabels
			playmedia "overlay_blank.png","PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			PuPlayer.LabelSet pBackglass,"AttractTitleL1","",1,"{'mt':2,'color':6655, 'size': 13, 'xpos': 50, 'xalign': 1, 'ypos': 21, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractNameL2","REPLAY AT",1,"{'mt':2,'color':28671, 'size': 15, 'xpos': 50, 'xalign': 1, 'ypos': 43, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractScoreL3",FormatNumber(ReplayAt,0),1,"{'mt':2,'color':28671, 'size': 10, 'xpos': 50, 'xalign': 1, 'ypos': 64, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractCreditsL4","Credits : "&Credits,1,"{'mt':2,'color':6655, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 90, 'yalign': 1}"

		Case 5
			clearosblabels
'			loadhs
			If Credits = 0 Then									'!!!! I think is not use ??
				MessageCredits = "insert credits"				'!!!! I think is not use ??
				DOF 200, DOFOff									'!!!! I think is not use ??
			Else												'!!!! I think is not use ??
				MessageCredits = "press start"					'!!!! I think is not use ??
				DOF 200, DOFOn									'!!!! I think is not use ??
			End If
			PuPlayer.LabelSet pBackglass,"AttractTitleL1","",1,"{'mt':2,'color':6655, 'size': 13, 'xpos': 50, 'xalign': 1, 'ypos': 21, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractNameL2","Credits : "&Credits,1,"{'mt':2,'color':28671, 'size': 15, 'xpos': 50, 'xalign': 1, 'ypos': 43, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractScoreL3",MessageCredits,1,"{'mt':2,'color':28671, 'size': 10, 'xpos': 50, 'xalign': 1, 'ypos': 64, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractCreditsL4","Credits : "&Credits,1,"{'mt':2,'color':6655, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 90, 'yalign': 1}"

		Case 6
			clearosblabels
			playmedia "overlay_blank.png","PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			PuPlayer.LabelSet pBackglass,"AttractTitleL1","grand champion",1,"{'mt':2,'color':6655, 'size': 13, 'xpos': 50, 'xalign': 1, 'ypos': 21, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractNameL2",HighScoreName(0),1,"{'mt':2,'color':28671, 'size': 15, 'xpos': 50, 'xalign': 1, 'ypos': 43, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractScoreL3",FormatNumber(HighScore(0),0),1,"{'mt':2,'color':28671, 'size': 10, 'xpos': 50, 'xalign': 1, 'ypos': 64, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractCreditsL4","Credits : "&Credits,1,"{'mt':2,'color':6655, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 90, 'yalign': 1}"

		Case 7
			clearhslabels
			clearosblabels
			playmedia "overlay_blank.png","PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			PuPlayer.LabelSet pBackglass,"AttractTitleL1","high score #1",1,"{'mt':2,'color':6655, 'size': 13, 'xpos': 50, 'xalign': 1, 'ypos': 21, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractNameL2",HighScoreName(1),1,"{'mt':2,'color':28671, 'size': 15, 'xpos': 50, 'xalign': 1, 'ypos': 43, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractScoreL3",FormatNumber(HighScore(1),0),1,"{'mt':2,'color':28671, 'size': 10, 'xpos': 50, 'xalign': 1, 'ypos': 64, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractCreditsL4","Credits : "&Credits,1,"{'mt':2,'color':6655, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 90, 'yalign': 1}"

		Case 8
			clearhslabels
			clearosblabels
			playmedia "overlay_blank.png","PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			PuPlayer.LabelSet pBackglass,"AttractTitleL1","high score #2",1,"{'mt':2,'color':6655, 'size': 13, 'xpos': 50, 'xalign': 1, 'ypos': 21, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractNameL2",HighScoreName(2),1,"{'mt':2,'color':28671, 'size': 15, 'xpos': 50, 'xalign': 1, 'ypos': 43, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractScoreL3",FormatNumber(HighScore(2),0),1,"{'mt':2,'color':28671, 'size': 10, 'xpos': 50, 'xalign': 1, 'ypos': 64, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractCreditsL4","Credits : "&Credits,1,"{'mt':2,'color':6655, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 90, 'yalign': 1}"

		Case 9
			clearhslabels
			clearosblabels
			playmedia "overlay_blank.png","PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			PuPlayer.LabelSet pBackglass,"AttractTitleL1","high score #3",1,"{'mt':2,'color':6655, 'size': 13, 'xpos': 50, 'xalign': 1, 'ypos': 21, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractNameL2",HighScoreName(3),1,"{'mt':2,'color':28671, 'size': 15, 'xpos': 50, 'xalign': 1, 'ypos': 43, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractScoreL3",FormatNumber(HighScore(3),0),1,"{'mt':2,'color':28671, 'size': 10, 'xpos': 50, 'xalign': 1, 'ypos': 64, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractCreditsL4","Credits : "&Credits,1,"{'mt':2,'color':6655, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 90, 'yalign': 1}"

		Case 10
			clearhslabels
			clearosblabels
			playmedia "overlay_blank.png","PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			PuPlayer.LabelSet pBackglass,"AttractTitleL1","high score #4",1,"{'mt':2,'color':6655, 'size': 13, 'xpos': 50, 'xalign': 1, 'ypos': 21, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractNameL2",HighScoreName(4),1,"{'mt':2,'color':28671, 'size': 15, 'xpos': 50, 'xalign': 1, 'ypos': 43, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractScoreL3",FormatNumber(HighScore(4),0),1,"{'mt':2,'color':28671, 'size': 10, 'xpos': 50, 'xalign': 1, 'ypos': 64, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractCreditsL4","Credits : "&Credits,1,"{'mt':2,'color':6655, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 90, 'yalign': 1}"

		Case 11
			clearhslabels
			clearosblabels
			playmedia "overlay_blank.png","PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			PuPlayer.LabelSet pBackglass,"AttractTitleL1","combo champion",1,"{'mt':2,'color':6655, 'size': 13, 'xpos': 50, 'xalign': 1, 'ypos': 21, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractNameL2",HighScoreComboChampionName,1,"{'mt':2,'color':28671, 'size': 15, 'xpos': 50, 'xalign': 1, 'ypos': 43, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractScoreL3",FormatNumber(HighScoreComboChampion,0),1,"{'mt':2,'color':28671, 'size': 10, 'xpos': 50, 'xalign': 1, 'ypos': 64, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractCreditsL4","Credits : "&Credits,1,"{'mt':2,'color':6655, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 90, 'yalign': 1}"

		Case 12
			clearhslabels
			clearosblabels
			playmedia "overlay_blank.png","PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			PuPlayer.LabelSet pBackglass,"AttractTitleL1","Knight Champion",1,"{'mt':2,'color':6655, 'size': 13, 'xpos': 50, 'xalign': 1, 'ypos': 21, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractNameL2",HighScoreKnightChampionName,1,"{'mt':2,'color':28671, 'size': 15, 'xpos': 50, 'xalign': 1, 'ypos': 43, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractScoreL3",FormatNumber(HighScoreKnightChampion,0),1,"{'mt':2,'color':28671, 'size': 10, 'xpos': 50, 'xalign': 1, 'ypos': 64, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractCreditsL4","Credits : "&Credits,1,"{'mt':2,'color':6655, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 90, 'yalign': 1}"

		Case 13
			clearhslabels
			clearosblabels
			playmedia "overlay_blank.png","PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			PuPlayer.LabelSet pBackglass,"AttractTitleL1","black castle champion",1,"{'mt':2,'color':6655, 'size': 13, 'xpos': 50, 'xalign': 1, 'ypos': 21, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractNameL2",HighScoreBlackCastleChampionName,1,"{'mt':2,'color':28671, 'size': 15, 'xpos': 50, 'xalign': 1, 'ypos': 43, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractScoreL3",FormatNumber(HighScoreBlackCastleChampion,0),1,"{'mt':2,'color':28671, 'size': 10, 'xpos': 50, 'xalign': 1, 'ypos': 64, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractCreditsL4","Credits : "&Credits,1,"{'mt':2,'color':6655, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 90, 'yalign': 1}"

		Case 14
			clearhslabels
			clearosblabels
			playmedia "overlay_blank.png","PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			PuPlayer.LabelSet pBackglass,"AttractTitleL1","loop champion",1,"{'mt':2,'color':6655, 'size': 13, 'xpos': 50, 'xalign': 1, 'ypos': 21, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractNameL2",HighScoreLoopChampionName,1,"{'mt':2,'color':28671, 'size': 15, 'xpos': 50, 'xalign': 1, 'ypos': 43, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractScoreL3",FormatNumber(HighScoreLoopChampion,0),1,"{'mt':2,'color':28671, 'size': 10, 'xpos': 50, 'xalign': 1, 'ypos': 64, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractCreditsL4","Credits : "&Credits,1,"{'mt':2,'color':6655, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 90, 'yalign': 1}"

		Case 15
			clearhslabels
			clearosblabels
			playmedia "overlay_blank.png","PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			PuPlayer.LabelSet pBackglass,"AttractTitleL1","war champion",1,"{'mt':2,'color':6655, 'size': 13, 'xpos': 50, 'xalign': 1, 'ypos': 21, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractNameL2",HighScoreWarChampionName,1,"{'mt':2,'color':28671, 'size': 15, 'xpos': 50, 'xalign': 1, 'ypos': 43, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractScoreL3",FormatNumber(HighScoreWarChampion,0),1,"{'mt':2,'color':28671, 'size': 10, 'xpos': 50, 'xalign': 1, 'ypos': 64, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractCreditsL4","Credits : "&Credits,1,"{'mt':2,'color':6655, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 90, 'yalign': 1}"

		Case 16
			clearhslabels
			clearosblabels
			playmedia "overlay_blank.png","PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			PuPlayer.LabelSet pBackglass,"AttractTitleL1","bonus champion",1,"{'mt':2,'color':6655, 'size': 13, 'xpos': 50, 'xalign': 1, 'ypos': 21, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractNameL2",HighScoreBonusChampionName,1,"{'mt':2,'color':28671, 'size': 15, 'xpos': 50, 'xalign': 1, 'ypos': 43, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractScoreL3",FormatNumber(HighScoreBonusChampion,0),1,"{'mt':2,'color':28671, 'size': 10, 'xpos': 50, 'xalign': 1, 'ypos': 64, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractCreditsL4","Credits : "&Credits,1,"{'mt':2,'color':6655, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 90, 'yalign': 1}"

		Case 17
			clearhslabels
			clearosblabels
			playmedia "overlay_blank.png","PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			PuPlayer.LabelSet pBackglass,"AttractTitleL1","triple knights mb champion",1,"{'mt':2,'color':6655, 'size': 13, 'xpos': 50, 'xalign': 1, 'ypos': 21, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractNameL2",HighScoreTripleKnightsMBChampionName,1,"{'mt':2,'color':28671, 'size': 15, 'xpos': 50, 'xalign': 1, 'ypos': 43, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractScoreL3",FormatNumber(HighScoreTripleKnightsMBChampion,0),1,"{'mt':2,'color':28671, 'size': 10, 'xpos': 50, 'xalign': 1, 'ypos': 64, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractCreditsL4","Credits : "&Credits,1,"{'mt':2,'color':6655, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 90, 'yalign': 1}"

		Case 18
			clearhslabels
			clearosblabels
			playmedia "overlay_blank.png","PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			PuPlayer.LabelSet pBackglass,"AttractTitleL1","catapult mb champion",1,"{'mt':2,'color':6655, 'size': 13, 'xpos': 50, 'xalign': 1, 'ypos': 21, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractNameL2",HighScoreCatapultMBChampionName,1,"{'mt':2,'color':28671, 'size': 15, 'xpos': 50, 'xalign': 1, 'ypos': 43, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractScoreL3",FormatNumber(HighScoreCatapultMBChampion,0),1,"{'mt':2,'color':28671, 'size': 10, 'xpos': 50, 'xalign': 1, 'ypos': 64, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractCreditsL4","Credits : "&Credits,1,"{'mt':2,'color':6655, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 90, 'yalign': 1}"

		Case 19
			clearhslabels
			clearosblabels
			playmedia "overlay_blank.png","PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			PuPlayer.LabelSet pBackglass,"AttractTitleL1","molten fire champion",1,"{'mt':2,'color':6655, 'size': 13, 'xpos': 50, 'xalign': 1, 'ypos': 21, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractNameL2",HighScoreMoltenFireChampionName,1,"{'mt':2,'color':28671, 'size': 15, 'xpos': 50, 'xalign': 1, 'ypos': 43, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractScoreL3",FormatNumber(HighScoreMoltenFireChampion,0),1,"{'mt':2,'color':28671, 'size': 10, 'xpos': 50, 'xalign': 1, 'ypos': 64, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractCreditsL4","Credits : "&Credits,1,"{'mt':2,'color':6655, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 90, 'yalign': 1}"

		Case 20
			clearhslabels
			clearosblabels
			playmedia "overlay_blank.png","PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			PuPlayer.LabelSet pBackglass,"AttractTitleL1","deep freeze champion",1,"{'mt':2,'color':6655, 'size': 13, 'xpos': 50, 'xalign': 1, 'ypos': 21, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractNameL2",HighScoreDeepFreezeChampionName,1,"{'mt':2,'color':28671, 'size': 15, 'xpos': 50, 'xalign': 1, 'ypos': 43, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractScoreL3",FormatNumber(HighScoreDeepFreezeChampion,0),1,"{'mt':2,'color':28671, 'size': 10, 'xpos': 50, 'xalign': 1, 'ypos': 64, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractCreditsL4","Credits : "&Credits,1,"{'mt':2,'color':6655, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 90, 'yalign': 1}"

		Case 21
			clearhslabels
			clearosblabels
			playmedia "overlay_blank.png","PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			PuPlayer.LabelSet pBackglass,"AttractTitleL1","mud bog champion",1,"{'mt':2,'color':6655, 'size': 13, 'xpos': 50, 'xalign': 1, 'ypos': 21, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractNameL2",HighScoreMudBogChampionName,1,"{'mt':2,'color':28671, 'size': 15, 'xpos': 50, 'xalign': 1, 'ypos': 43, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractScoreL3",FormatNumber(HighScoreMudBogChampion,0),1,"{'mt':2,'color':28671, 'size': 10, 'xpos': 50, 'xalign': 1, 'ypos': 64, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractCreditsL4","Credits : "&Credits,1,"{'mt':2,'color':6655, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 90, 'yalign': 1}"

		Case 22
			clearhslabels
			clearosblabels
			playmedia "overlay_blank.png","PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			PuPlayer.LabelSet pBackglass,"AttractTitleL1","wicked cavern champion",1,"{'mt':2,'color':6655, 'size': 13, 'xpos': 50, 'xalign': 1, 'ypos': 21, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractNameL2",HighScoreWickedCavernChampionName,1,"{'mt':2,'color':28671, 'size': 15, 'xpos': 50, 'xalign': 1, 'ypos': 43, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractScoreL3",FormatNumber(HighScoreWickedCavernChampion,0),1,"{'mt':2,'color':28671, 'size': 10, 'xpos': 50, 'xalign': 1, 'ypos': 64, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractCreditsL4","Credits : "&Credits,1,"{'mt':2,'color':6655, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 90, 'yalign': 1}"

		Case 23
			clearhslabels
			clearosblabels
			playmedia "overlay_blank.png","PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			PuPlayer.LabelSet pBackglass,"AttractTitleL1","Burning Sands Champion",1,"{'mt':2,'color':6655, 'size': 13, 'xpos': 50, 'xalign': 1, 'ypos': 21, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractNameL2",HighScoreBurningSandsChampionName,1,"{'mt':2,'color':28671, 'size': 15, 'xpos': 50, 'xalign': 1, 'ypos': 43, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractScoreL3",FormatNumber(HighScoreBurningSandsChampion,0),1,"{'mt':2,'color':28671, 'size': 10, 'xpos': 50, 'xalign': 1, 'ypos': 64, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractCreditsL4","Credits : "&Credits,1,"{'mt':2,'color':6655, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 90, 'yalign': 1}"

		Case 24
			clearhslabels
			clearosblabels
			playmedia "overlay_blank.png","PuPOverlays",pBackGlass,"cineon",100,"",1,compteuri  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			PuPlayer.LabelSet pBackglass,"AttractTitleL1","super champion",1,"{'mt':2,'color':6655, 'size': 13, 'xpos': 50, 'xalign': 1, 'ypos': 21, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractNameL2",HighScoreSuperChampionName,1,"{'mt':2,'color':28671, 'size': 15, 'xpos': 50, 'xalign': 1, 'ypos': 43, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractScoreL3",FormatNumber(HighScoreSuperChampion,0),1,"{'mt':2,'color':28671, 'size': 10, 'xpos': 50, 'xalign': 1, 'ypos': 64, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"AttractCreditsL4","Credits : "&Credits,1,"{'mt':2,'color':6655, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 90, 'yalign': 1}"

		Case 25
			clearhslabels
			clearosblabels
			'DOF 499, DOFPulse
			pupevent 499
			DelayVideoAttract = 3
			playclear pMusic
'			Last Player --> Player 1			| 00

		Case 26
			clearhslabels
			clearosblabels
			playclear pMusic
'			Last Player --> Player 1			| 00

			dim attractlooprnd
			attractlooprnd=RndNum(1,6)
			select case attractlooprnd
				case 1:playmedia "ATTRACT_LOOP1.mp4","Attract loop",pBackGlass,"cineon",100,"",1,compteuri : DelayVideoAttract = 18 '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				case 2:playmedia "ATTRACT_LOOP2.mp4","Attract loop",pBackGlass,"cineon",100,"",1,compteuri : DelayVideoAttract = 16 '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				case 3:playmedia "ATTRACT_LOOP3.mp4","Attract loop",pBackGlass,"cineon",100,"",1,compteuri : DelayVideoAttract = 16 '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				case 4:playmedia "ATTRACT_LOOP1_BW.mp4","Attract loop",pBackGlass,"cineon",100,"",1,compteuri : DelayVideoAttract = 18 '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				case 5:playmedia "ATTRACT_LOOP2_BW.mp4","Attract loop",pBackGlass,"cineon",100,"",1,compteuri : DelayVideoAttract = 16 '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				case 6:playmedia "ATTRACT_LOOP3_BW.mp4","Attract loop",pBackGlass,"cineon",100,"",1,compteuri : DelayVideoAttract = 16 '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			end Select
	
		Case 27
			clearhslabels
			clearosblabels
			playclear pMusic
'			Last Player --> Player 1			| 00
			dim tutopremrnd
			tutopremrnd=RndNum(1,4)
			select case tutopremrnd
				case 1:playmedia "Black Knight Premium - Catapult Multiball.mp4","Tutorial Premium",pBackGlass,"cineon",100,"",1,compteuri : DelayVideoAttract = 35 '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				case 2:playmedia "Black Knight Premium - Lightning Wheel Modes.mp4","Tutorial Premium",pBackGlass,"cineon",100,"",1,compteuri : DelayVideoAttract = 46 '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				case 3:playmedia "Black Knight Premium - Triple Knights Challenge Multiball.mp4","Tutorial Premium",pBackGlass,"cineon",100,"",1,compteuri : DelayVideoAttract = 44 '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				case 4:playmedia "Black Knight Premium - War Hurry-ups.mp4","Tutorial Premium",pBackGlass,"cineon",100,"",1,compteuri : DelayVideoAttract = 55 '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)

			end Select

		Case 28
			playclear pBackglass
			introposition = 0
			clearhslabels
			clearosblabels
			'PuPlayer.playpause 4
			'playmedia "BlackKnight_AttractModeLoopingBG.mp4","Background looping",pBackglass,"",53000,"",1,10
'			DOF 504, DOFPulse
			pupevent 504
			playclear pMusic
	End Select
	End Sub

	sub clearosblabels
		PuPlayer.LabelSet pBackglass,"AttractTitleL1","",1,"{'mt':2,'color':6655, 'size': 13, 'xpos': 50, 'xalign': 1, 'ypos': 21, 'yalign': 1}"
		PuPlayer.LabelSet pBackglass,"AttractNameL2","",1,"{'mt':2,'color':28671, 'size': 15, 'xpos': 50, 'xalign': 1, 'ypos': 43, 'yalign': 1}"
		PuPlayer.LabelSet pBackglass,"AttractScoreL3","",1,"{'mt':2,'color':28671, 'size': 10, 'xpos': 50, 'xalign': 1, 'ypos': 64, 'yalign': 1}"
		PuPlayer.LabelSet pBackglass,"AttractCreditsL4","",1,"{'mt':2,'color':6655, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 90, 'yalign': 1}"

		PuPlayer.LabelSet pBackglass,"GameOverPlayer1","",1,"{'mt':2,'color':6655, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 90, 'yalign': 1}"
		PuPlayer.LabelSet pBackglass,"ScoreGameOverPlayer1","",1,"{'mt':2,'color':6655, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 90, 'yalign': 1}"
		PuPlayer.LabelSet pBackglass,"GameOverPlayer2","",1,"{'mt':2,'color':6655, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 90, 'yalign': 1}"
		PuPlayer.LabelSet pBackglass,"ScoreGameOverPlayer2","",1,"{'mt':2,'color':6655, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 90, 'yalign': 1}"
		PuPlayer.LabelSet pBackglass,"GameOverPlayer3","",1,"{'mt':2,'color':6655, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 90, 'yalign': 1}"
		PuPlayer.LabelSet pBackglass,"ScoreGameOverPlayer3","",1,"{'mt':2,'color':6655, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 90, 'yalign': 1}"
		PuPlayer.LabelSet pBackglass,"GameOverPlayer4","",1,"{'mt':2,'color':6655, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 90, 'yalign': 1}"
		PuPlayer.LabelSet pBackglass,"ScoreGameOverPlayer4","",1,"{'mt':2,'color':6655, 'size': 6, 'xpos': 50, 'xalign': 1, 'ypos': 90, 'yalign': 1}"

		PuPlayer.LabelSet pBackglass,"wh1n","",1,""
		PuPlayer.LabelSet pBackglass,"wh1s","",1,""
		PuPlayer.LabelSet pBackglass,"wh2n","",1,""
		PuPlayer.LabelSet pBackglass,"wh2s","",1,""
		PuPlayer.LabelSet pBackglass,"wh3n","",1,""
		PuPlayer.LabelSet pBackglass,"wh3s","",1,""
		PuPlayer.LabelSet pBackglass,"wh4n","",1,""
		PuPlayer.LabelSet pBackglass,"wh4s","",1,""
		PuPlayer.LabelSet pBackglass,"wh5n","",1,""
		PuPlayer.LabelSet pBackglass,"wh5s","",1,""
		PuPlayer.LabelSet pBackglass,"wh6n","",1,""
		PuPlayer.LabelSet pBackglass,"wh6s","",1,""
		PuPlayer.LabelSet pBackglass,"wh7n","",1,""
		PuPlayer.LabelSet pBackglass,"wh7s","",1,""
		PuPlayer.LabelSet pBackglass,"wh8n","",1,""
		PuPlayer.LabelSet pBackglass,"wh8s","",1,""
		PuPlayer.LabelSet pBackglass,"wh9n","",1,""
		PuPlayer.LabelSet pBackglass,"wh9s","",1,""
		PuPlayer.LabelSet pBackglass,"wh10n","",1,""
		PuPlayer.LabelSet pBackglass,"wh10s","",1,""
		PuPlayer.LabelSet pBackglass,"ah1n","",1,""
		PuPlayer.LabelSet pBackglass,"ah1s","",1,""
		PuPlayer.LabelSet pBackglass,"ah2n","",1,""
		PuPlayer.LabelSet pBackglass,"ah2s","",1,""
		PuPlayer.LabelSet pBackglass,"ah3n","",1,""
		PuPlayer.LabelSet pBackglass,"ah3s","",1,""
		PuPlayer.LabelSet pBackglass,"ah4n","",1,""
		PuPlayer.LabelSet pBackglass,"ah4s","",1,""
		PuPlayer.LabelSet pBackglass,"ah5n","",1,""
		PuPlayer.LabelSet pBackglass,"ah5s","",1,""
		PuPlayer.LabelSet pBackglass,"ah6n","",1,""
		PuPlayer.LabelSet pBackglass,"ah6s","",1,""
		PuPlayer.LabelSet pBackglass,"ah7n","",1,""
		PuPlayer.LabelSet pBackglass,"ah7s","",1,""
		PuPlayer.LabelSet pBackglass,"ah8n","",1,""
		PuPlayer.LabelSet pBackglass,"ah8s","",1,""
		PuPlayer.LabelSet pBackglass,"ah9n","",1,""
		PuPlayer.LabelSet pBackglass,"ah9s","",1,""
		PuPlayer.LabelSet pBackglass,"ah10n","",1,""
		PuPlayer.LabelSet pBackglass,"ah10s","",1,""
	end Sub

	sub clearhslabels
		PuPlayer.LabelSet pBackglass,"HighScore","",1,""
		PuPlayer.LabelSet pBackglass,"HighScore2","",1,""
		PuPlayer.LabelSet pBackglass,"HighScoreL1","",1,""
		PuPlayer.LabelSet pBackglass,"HighScoreL2","",1,""
		PuPlayer.LabelSet pBackglass,"HighScoreL3","",1,""
		PuPlayer.LabelSet pBackglass,"HighScore","",1,""
		PuPlayer.LabelSet pBackglass,"HighScore2","",1,""
		PuPlayer.LabelSet pBackglass,"HighScoreL1","",1,""
		PuPlayer.LabelSet pBackglass,"HighScoreL2","",1,""
		PuPlayer.LabelSet pBackglass,"HighScoreL3","",1,""
		PuPlayer.LabelSet pBackglass,"high1name","",1,""
		PuPlayer.LabelSet pBackglass,"high1score","",1,""
		PuPlayer.LabelSet pBackglass,"high2name","",1,""
		PuPlayer.LabelSet pBackglass,"high2score","",1,""
		PuPlayer.LabelSet pBackglass,"high3name","",1,""
		PuPlayer.LabelSet pBackglass,"high3score","",1,""
		PuPlayer.LabelSet pBackglass,"high4name","",1,""
		PuPlayer.LabelSet pBackglass,"high4score","",1,""
		PuPlayer.LabelSet pBackglass,"high5name","",1,""
		PuPlayer.LabelSet pBackglass,"high5score","",1,""
	end Sub

	Sub intromover_timer
		introtime = introtime + 1
		If introposition = 1 Then
			If introtime = 1 Then
				DMDintroloop
			End If
		End If
		If introposition = 2 Then
			If introtime = 2 Then
				DMDintroloop
			End If
		End If
		If introposition = 3 Then
			If introtime = 5 Then
				DMDintroloop
			End If
		End If
		If introposition = 4 Then
			If introtime = 2 Then
				DMDintroloop
			End If
		End If
		If introposition = 5 Then
			If introtime = 3 Then
				DMDintroloop
			End If
		End If
		If introposition = 6 Then
			If introtime = 2 Then
				DMDintroloop
			End If
		End If
		If introposition = 7 Then
			If introtime = 2 Then
				DMDintroloop
			End If
		End If
		If introposition = 8 Then
			If introtime = 2 Then
				DMDintroloop
			End If
		End If
		If introposition = 9 Then
			If introtime = 2 Then
				DMDintroloop
			End If
		End If
		If introposition = 10 Then
			If introtime = 2 Then
				DMDintroloop
			End If
		End If
		If introposition = 11 Then
			If introtime = 2 Then
				DMDintroloop
			End If
		End If
		If introposition = 12 Then
			If introtime = 2 Then
				DMDintroloop
			End If
		End If
		If introposition = 13 Then
			If introtime = 2 Then
				DMDintroloop
			End If
		End If		
		If introposition = 14 Then
			If introtime = 2 Then
				DMDintroloop
			End If
		End If
		If introposition = 15 Then
			If introtime = 2 Then
				DMDintroloop
			End If
		End If		
		If introposition = 16 Then
			If introtime = 2 Then
				DMDintroloop
			End If
		End If
		If introposition = 17 Then
			If introtime = 2 Then
				DMDintroloop
			End If
		End If		
		If introposition = 18 Then
			If introtime = 2 Then
				DMDintroloop
			End If
		End If
		If introposition = 19 Then
			If introtime = 2 Then
				DMDintroloop
			End If
		End If		
		If introposition = 20 Then
			If introtime = 2 Then
				DMDintroloop
			End If
		End If
		If introposition = 21 Then
			If introtime = 2 Then
				DMDintroloop
			End If
		End If		
		If introposition = 22 Then
			If introtime = 2 Then
				DMDintroloop
			End If
		End If
		If introposition = 23 Then
			If introtime = 2 Then
				DMDintroloop
			End If
		End If		
		If introposition = 24 Then
			If introtime = 2 Then
				DMDintroloop
			End If
		End If
		If introposition = 25 Then
			If introtime = DelayVideoAttract Then 
				DMDintroloop
			End If
		End If
		If introposition = 26 Then
			If introtime = DelayVideoAttract Then 
				DMDintroloop
			End If
		End If
		If introposition = 27 Then
			If introtime = DelayVideoAttract Then 
				DMDintroloop
			End If
		End If
		If introposition = 28 Then
			introposition = 0
			DMDintroloop	
		End If
		If introposition = 0 Then
				DMDintroloop
		End If
	End Sub

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  GAME STARTING & RESETS
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' 


	Sub Game_Init() 'called at the start of a new game
		playclear pBackglass
'		inmermulti = 0
		setlightvalues
'		closemaze
		cineon = 0
		Dim i
		For i = 1 to 4
			bumplvl(i) = 0
			currentcombo(i) = 0
			totalcombo(i) = 0 
			dragonlock(i) = 0
			dragonjacks(i) = 0
			playerspins(i) = 0
			mermultispins(i) = 0
			wandlocks(i) = 0
			rrhits(i) = 0
			lrhits(i) = 0
			mazelocks(i) = 0
			mazejacks(i) = 0
			wandjacks(i) = 0
			potions(i) = 0
			pbumps(i) = 0
			totalbumps(i) = 0
			goblets(i) = 0
			deshit(i) = 0
			modescompleted(i) = 0
			modeeb(i) = 0
			spinlvl(i) = 0
			spinnum(i) = 75
			delvl(i) = 0
			potready(i) = 0
			potionlevel(i) = 0
			merlinsecond(i) = 0
'			L131.State(i) = 0
'			L132.State(i) = 0
'			L133.State(i) = 0

		Next
'		lrflashtime.Enabled = False
		bExtraBallWonThisBall = False
		bballfirstball = 1
		PuPlayer.LabelSet pBackglass,"high1name","",1,""
		PuPlayer.LabelSet pBackglass,"high1score","",1,""
		PuPlayer.LabelSet pBackglass,"high2name","",1,""
		PuPlayer.LabelSet pBackglass,"high2score","",1,""
		PuPlayer.LabelSet pBackglass,"high3name","",1,""
		PuPlayer.LabelSet pBackglass,"high3score","",1,""
		PuPlayer.LabelSet pBackglass,"high4name","",1,""
		PuPlayer.LabelSet pBackglass,"high4score","",1,""
		PuPlayer.LabelShowPage pBackglass,1,0,""
		pUpdateScores
		PuPlayer.LabelSet pBackglass,"Play1","PLAYER 1",1,"{'mt':2,'color':16777215}"
		PuPlayer.LabelSet pBackglass,"notetitle","",1,""
		PuPlayer.LabelSet pBackglass,"notecopy","",1,""

		resetbackglass
		clearosblabels
		clearhslabels
		'PuPlayer.playlistplayex pBackglass,"backglass","base.mp4",0,1  'should be an attract background (no text is displayed)
		'PuPlayer.SetBackground pBackglass,1	
		If help = 1 Then
			helpful.enabled = 1
		end if
			ActionWhenHitOneOfThreeWay
	End Sub

	sub	helpful_timer
		helppos = helppos + 1
		if bMultiBallMode = false Then
			if inamode = 0 Then
				if cineon = 0 then
			select case helppos
			case 1 : 'PuPlayer.playlistplayex pCallouts,"audiocallouts","shoot the bumpers to learn a spell.mp3",vovol,1
						'playmedia "brilliant that makes me feel loads better.mp4","videodrains",pBackglass,"cineon",6000,"dropwall",1,1  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'						pupDMDDisplay "-","Shoot Bumpers^For Video Mode",dmdnote,3,0,10

			case 2 : 'PuPlayer.playlistplayex pCallouts,"audiocallouts","shoot the spinners to start lake rescue.mp3",vovol,1
						playmedia "shoot the spinners to start lake rescue.mp3","audiocallouts",pCallouts,"",4000,"",1,1  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'						pupDMDDisplay "-","Shoot Spinners^For Lake Lock",dmdnote,3,0,10

			case 3 : 'PuPlayer.playlistplayex pCallouts,"audiocallouts","hit the targets to light a mode.mp3",vovol,1
						playmedia "hit the targets to light a mode.mp3","audiocallouts",pCallouts,"",6000,"",1,1  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)	
'						pupDMDDisplay "-","Spell Potter^To Light Mode",dmdnote,3,0,10

			case 4 : 'PuPlayer.playlistplayex pCallouts,"audiocallouts","hit the ramps to start maze multiball.mp3",vovol,1
						playmedia "hit the ramps to start maze multiball.mp3","audiocallouts",pCallouts,"",6000,"",1,1  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'						pupDMDDisplay "-","Hit Ramps^To Open Maze",dmdnote,3,0,10

			case 5 : 'PuPlayer.playlistplayex pCallouts,"audiocallouts","hit the cauldron to make a potion.mp3",vovol,1
						playmedia "hit the cauldron to make a potion.mp3","audiocallouts",pCallouts,"",6000,"",1,1  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'						pupDMDDisplay "-","Stir Cauldron^To Make a Potion",dmdnote,3,0,10

			case 6 : 'PuPlayer.playlistplayex pCallouts,"audiocallouts","hit the captive ball to light lock.mp3",vovol,1
						playmedia "hit the captive ball to light lock.mp3","audiocallouts",pCallouts,"",6000,"",1,1  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'						pupDMDDisplay "-","Hit Captive Ball^To Battle Dragon",dmdnote,3,0,10

			case 7 : 'PuPlayer.playlistplayex pCallouts,"audiocallouts","get combos for wand multiball.mp3",vovol,1
						playmedia "get combos for wand multiball.mp3","audiocallouts",pCallouts,"",6000,"",1,1  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
						helppos = 0
'						pupDMDDisplay "-","Get Combos^for Wand Multi",dmdnote,3,0,10
			end Select
			end if
			end if
		end If
	end Sub

	Sub StopEndOfBallMode()              'this sub is called after the last ball is drained
		'ResetSkillShotTimer_Timer
		ResetSkillShotTimer.Enabled = True
		If Tilted = True Then
			Tilted = False
			DisableTable False
			tilttime = 0
			tilttableclear.enabled = False
			TiltRecoveryTimer.Enabled = False 'start the Tilt delay to check for all the balls to be drained
			bBallSaverReady = True
			bBallSaverActive = False
		End If	
		If StopRansom = True Then
			StopRansom = False
			DisableTable False
			bBallSaverReady = True
			bBallSaverActive = False
		End If
	End Sub

	Sub ResetNewBallVariables()
		'relighttable
'		wandclose
		'PuPlayer.playlistplayex pMusic,songs,"",sndtrkvol,1
		'playmedia "",songs,pMusic,"",0,"",1,4  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		'playbgaudio
		'PuPlayer.SetLoop 4,1
	End Sub

	Sub TurnOffPlayfieldLights()
		Dim a
		For each a in alights
			a.State = 0
		Next
	End Sub

	Sub TurnOffmultiLights()
		Dim a
		For each a in alights
			a.State = 0
		Next
		If pt2(CurrentPlayer) = 1 Then
			t2.state = 1
		End If
	End Sub

	Sub ResetNewBallLights() 'turn on or off the needed lights before a new ball is released
		TurnOffPlayfieldLights()
		currentplayerbackglass
	End Sub

	Sub startamultiball
		Dim waittime
		waittime = 1000
		'vpmtimer.addtimer waittime, "closeupshop'"
		closeupshop
	End Sub
	
	Sub closeupshop

	End Sub

	Sub endamultiball

	End Sub


	Sub UpdateSkillShot() 'Updates the skillshot light
		l7.State = 2
	End Sub

	Sub SkillshotOff_Hit 'trigger to stop the skillshot due to a weak plunger shot
		If bSkillShotReady Then
			ResetSkillShotTimer_Timer
		End If
	End Sub

	Sub ResetSkillShotTimer_Timer 'timer to reset the skillshot lights & variables
		'ResetSkillShotTimer.Enabled = 0
		ResetSkillShotTimer.Enabled = False
		bSkillShotReady = False
			'If l7.State = 2 then l7.State = 0
	End Sub


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  RULES HELPER FOR YOUR BACKGLASS
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' 



'*******************************************************************************************************************************************
'*
'*
'*
'*														MUSIC PARTS
'*
'*
'*
'*******************************************************************************************************************************************
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'   Background Audio
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'Dim SWNumber

Sub StartPlayMusic_timer()
'	StartPlayMusic.Enabled=False
'	playmedia MusicSelected,"Audiomusic",pCallouts,"",0,"",1,10  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
End Sub

Sub MusicCheck(SWNumbered)
	musicpriority = musicpriority + 1
'	DOF 6, DOFPulse
	pupevent 6
	PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 4, ""FN"":11, ""VL"":"&sndtrkvol&" }"
	If SWNumbered = 0 Then
		Gamerover_flag = True
		'MusicSelected = ""
		PuPlayer.playstop pCallouts
	End if
	If SWNumbered = 1 And Knight_challenge_flag = False And CatapultModeFlag = False Then
		MusicSelected = "Sound-0x099E - intro.mp3" 'just for test'
		If CurrentMissionFlag(CurrentPlayer) = 1 Then
			If MudBog(CurrentPlayer) = 2 Then MusicSelected = "Sound-0x09C8.mp3" End If 'Sound-0x09C8.mp3
			If MoltenFire(CurrentPlayer) = 2 Then MusicSelected = "Sound-0x09C7.mp3" End If'Sound-0x09C7
			If BurningSands(CurrentPlayer) = 2 Then MusicSelected = "Sound-0x0974.mp3" End If 'Sound-0x0974.mp3
			If WickedCavern(CurrentPlayer) = 2 Then MusicSelected = "Sound-0x0991.mp3" End If  'Sound-0x0991
			If DeepFreeze(CurrentPlayer) = 2 Then MusicSelected = "Sound-0x099B.mp3" End If 'Sound-0x099B
			If BlackCastle(CurrentPlayer) = 2 Then MusicSelected = "Sound-0x09BD.mp3" End If 'Sound-0x09BD'
			If LastChanceStart = True Then MusicSelected = "Sound-0x096F.mp3" End If 'Sound-0x096F'
		End If
	End If 
	If SWNumbered = 22 Then 
		MusicSelected = "Sound-0x099E - intro.mp3"	'Start game'
		If CurrentMissionFlag(CurrentPlayer) = 1 Then
			If MudBog(CurrentPlayer) = 2 Then MusicSelected = "Sound-0x09C8.mp3" End If 'Sound-0x09C8.mp3
			If MoltenFire(CurrentPlayer) = 2 Then MusicSelected = "Sound-0x09C7.mp3" End If'Sound-0x09C7
			If BurningSands(CurrentPlayer) = 2 Then MusicSelected = "Sound-0x0974.mp3" End If 'Sound-0x0974.mp3
			If WickedCavern(CurrentPlayer) = 2 Then MusicSelected = "Sound-0x0991.mp3" End If  'Sound-0x0991
			If DeepFreeze(CurrentPlayer) = 2 Then MusicSelected = "Sound-0x099B.mp3" End If 'Sound-0x099B
			If BlackCastle(CurrentPlayer) = 2 Then MusicSelected = "Sound-0x09BD.mp3" End If 'Sound-0x09BD'
			If LastChanceStart = True Then MusicSelected = "Sound-0x096F.mp3" End If 'Sound-0x096F'
		End If
		If Knight_challenge_flag = True Then
			MusicSelected = "Sound-0x0988.mp3"
		End If
		If CatapultModeFlag = True Then
			MusicSelected = "Sound-0x09AC.mp3"
		End If
	End If
	If SWNumbered = 45 Then
		MusicSelected = "Sound-0x09B3.mp3"
	End If
	If SWNumbered = 46 Then
		MusicSelected = "Sound-0x09CE.mp3"
	End If
	If SWNumbered = 85 Then
		MusicSelected = "Sound-0x09AC.mp3"
	End If
	If SWNumbered = 66 Then						'WarHurryPhase 1
		MusicSelected = "Sound-0x0980.mp3"
	End If
	If SWNumbered = 200 Then						'WarHurryPhase 2
		MusicSelected = "Sound-0x09CA.mp3"
	End If

	If SWNumbered = 60 And Retromode = 0 Then
	'	endmusic
	'	playclear pCallouts
	'	DOF 6, DOFPulse
		If Knight_challenge_flag = True Then MusicSelected = "Sound-0x0988.mp3" End If 'Sound-0x0988.mp3
		If MudBog(CurrentPlayer) = 1 Then MusicSelected = "Sound-0x0989.mp3" End If 'Sound-0x0989.mp3
		If MoltenFire(CurrentPlayer) = 1 Then MusicSelected = "Sound-0x09BF.mp3" End If'Sound-0x09BF
		If BurningSands(CurrentPlayer) = 1 Then MusicSelected = "Sound-0x097c.mp3" End If 'Sound-0x097c.mp3
		If WickedCavern(CurrentPlayer) = 1 Then MusicSelected = "Sound-0x09BB.mp3" End If  'Sound-0x09BB
		If DeepFreeze(CurrentPlayer) = 1 Then MusicSelected = "Sound-0x0983.mp3" End If 'Sound-0x0983
		If BlackCastle(CurrentPlayer) = 1 Then MusicSelected = "Sound-0x09B5.mp3" End If 'Sound-0x09B5
		If LastChanceStart = True Then MusicSelected = "Sound-0x096F.mp3" End If 'Sound-0x096F'
	End If

	If SWNumbered = 86 And Retromode = 0 And CatapultModeFlag = False And Knight_challenge_flag = False Then				'Gate 002
		MusicSelected="Sound-0x09BA.mp3" 
		If MudBog(CurrentPlayer) = 2 Then MusicSelected = "Sound-0x0989.mp3" End If 'Sound-0x0989.mp3
		If MoltenFire(CurrentPlayer) = 2 Then MusicSelected = "Sound-0x09BF.mp3" End If'Sound-0x09BF
		If BurningSands(CurrentPlayer) = 2 Then MusicSelected = "Sound-0x097c.mp3" End If 'Sound-0x097c.mp3
		If WickedCavern(CurrentPlayer) = 2 Then MusicSelected = "Sound-0x09BB.mp3" End If  'Sound-0x09BB
		If DeepFreeze(CurrentPlayer) = 2 Then MusicSelected = "Sound-0x0983.mp3" End If 'Sound-0x0983
		If BlackCastle(CurrentPlayer) = 2 Then MusicSelected = "" End If ''
		If LastChanceStart = True Then MusicSelected = "Sound-0x096F.mp3" End If 'Sound-0x096F'
		If LastChanceFlag = True And LastChanceSuccess = False And CurrentMissionFlag(CurrentPlayer) = 0 Then PuPlayer.playstop pCallouts End If	'Stop
	End If
	If SWNumbered = 777 Then
		MusicSelected = "Sound-0x096F.mp3"
	End If
'	playmedia MusicSelected,"Audiomusic",pCallouts,"",1000,"",1,musicpriority '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	playmedia MusicSelected,songs,pCallouts,"",0,"",1,musicpriority
	If Gamerover_flag = False Then
		If OldMusicSelected <> MusicSelected Then
			playmedia MusicSelected,"Audiomusic",pCallouts,"",1000,"",1,musicpriority '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)	
			PuPlayer.SetLoop 6,1
		End If
	End If

	OldMusicSelected = MusicSelected
End Sub





Sub Table1_MusicDone
	If Gamerover_flag = False Then
		'PlayMusic MusicSelected
		playmedia MusicSelected,"Audiomusic",pCallouts,"",0,"",1,10  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	Else
		'endmusic
		'musicpriority = musicpriority + 1
		'DOF 6, DOFPulse
		'playmedia MusicSelected,"Audiomusic",pCallouts,"",1000,"",1,musicpriority '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)

		MusicCheck 0
	End If
End Sub


	sub playbgaudio
'		debug.print "playbgaudio "
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 4, ""FN"":11, ""VL"":"&sndtrkvol&" }"
		'PuPlayer.setvolume pMusic sndtrkvol
		dim trkpik
		if sndtrk = 1 Then
			trkpik=RndNum(1,4)
			select case trkpik
				case 1:playmedia "01. The Story Continues.mp3",songs,pMusic,"",0,"",1,11
				case 2:playmedia "05. Foreign Visitors Arrive.mp3",songs,pMusic,"",0,"",1,11
				case 3:playmedia "14. Underwater Secrets.mp3",songs,pMusic,"",0,"",1,11
				case 4:playmedia "harry sees dragons.mp3",songs,pMusic,"",0,"",1,11
			end Select
		Else
			trkpik=RndNum(1,29)
			select case trkpik
				case 1:playmedia "Art Brut - Emily Kane.mp3",songs,pMusic,"",0,"",1,10
				case 2:playmedia "Belle And Sebastian - Get Me Away From Here I Am Dying.mp3",songs,pMusic,"",0,"",1,10
				case 3:playmedia "James - She's a star.mp3",songs,pMusic,"",0,"",1,10
				case 4:playmedia "Depeche Mode - Enjoy The Silence (Lyrics on Screen) [HD].mp3",songs,pMusic,"",0,"",1,10
				case 5:playmedia "Oasis - Wonderwall (Official Video).mp3",songs,pMusic,"",0,"",1,10
				case 6:playmedia "Feeder - High.mp3",songs,pMusic,"",0,"",1,10
				case 7:playmedia "Franz Ferdinand - Take Me Out.mp3",songs,pMusic,"",0,"",1,10
				case 8:playmedia "James - She's a star.mp3",songs,pMusic,"",0,"",1,10
				case 9:playmedia "Keane - Somewhere Only We Know (Official Music Video).mp3",songs,pMusic,"",0,"",1,10
				case 10:playmedia "Nada Surf - Happy kid & lyrics.mp3",songs,pMusic,"",0,"",1,10
				case 11:playmedia "Oasis   Don't look back in anger _ Canon in D _ Lyrics.mp3",songs,pMusic,"",0,"",1,10
				case 12:playmedia "Primitive Radio Gods - Standing Outside A Broken Phone Booth With Money In My Hand.mp3",songs,pMusic,"",0,"",1,10
				case 13:playmedia "Oasis - Wonderwall (Official Video).mp3",songs,pMusic,"",0,"",1,10
				case 14:playmedia "Primitive Radio Gods - Standing Outside A Broken Phone Booth With Money In My Hand.mp3",songs,pMusic,"",0,"",1,10
				case 15:playmedia "Radiohead - 02 The Bends.mp3",songs,pMusic,"",0,"",1,10
				case 16:playmedia "Radiohead - High & Dry.mp3",songs,pMusic,"",0,"",1,10
				case 17:playmedia "radiohead_-_karma-police.mp3",songs,pMusic,"",0,"",1,10
				case 18:playmedia "Stereo MC's  ?Connected_.mp3",songs,pMusic,"",0,"",1,10
				case 19:playmedia "Supergrass - Alright.mp3",songs,pMusic,"",0,"",1,10
				case 20:playmedia "The Beta Band - She's the One.mp3",songs,pMusic,"",0,"",1,10
				case 21:playmedia "The Breeders - Cannonball.mp3",songs,pMusic,"",0,"",1,10
				case 22:playmedia "The Coral - Dreaming Of You HQ.mp3",songs,pMusic,"",0,"",1,10
				case 23:playmedia "The Cure - Boys Don't Cry (1979).mp3",songs,pMusic,"",0,"",1,10
				case 24:playmedia "The Cure - Friday I' m in Love [ Official Edition ].mp3",songs,pMusic,"",0,"",1,10
				case 25:playmedia "The Libertines - Can't Stand Me Now (Official Video).mp3",songs,pMusic,"",0,"",1,10
				case 26:playmedia "The Prodigy - Firestarter (Official Video).mp3",songs,pMusic,"",0,"",1,10
				case 27:playmedia "The Verve - Bitter Sweet Symphony (Official Video).mp3",songs,pMusic,"",0,"",1,10
				case 28:playmedia "The Verve - Bitter Sweet Symphony (Official Video).mp3",songs,pMusic,"",0,"",1,10
				case 29:playmedia "Travis - Why Does It Always Rain on Me.mp3",songs,pMusic,"",0,"",1,10
			end Select
		end if
		PuPlayer.SetLoop 4,1
	end Sub

'*******************************************************************************************************************************************
'*
'*														END MUSIC PARTS
'*
'*******************************************************************************************************************************************

Sub WickedCavernCheckLeft_Timer()
	WickedCavernCheckLeft.Enabled=False
	If WickedCavernCheckCountRandomLeft(CurrentPlayer) = 0 Then 
		RandomL01(CurrentPlayer) = randomLeft
		WickedCavernCheckCountRandomLeft(CurrentPlayer) = WickedCavernCheckCountRandomLeft(CurrentPlayer) +1
		CheckLamp.Enabled = True
	ElseIf WickedCavernCheckCountRandomLeft(CurrentPlayer) = 1 Then 
		If RandomL01(CurrentPlayer) = randomLeft Then
			WickedCavernRandomLeftLight
		Else
			RandomL02(CurrentPlayer) = randomLeft 
			WickedCavernCheckCountRandomLeft(CurrentPlayer) = WickedCavernCheckCountRandomLeft(CurrentPlayer) +1
		End If
	ElseIf WickedCavernCheckCountRandomLeft(CurrentPlayer) = 2 Then 
		If RandomL01(CurrentPlayer) = randomLeft Or RandomL02(CurrentPlayer) = randomLeft Then
			WickedCavernRandomLeftLight
		Else
			RandomL03(CurrentPlayer) = randomLeft 
			WickedCavernCheckCountRandomLeft(CurrentPlayer) = WickedCavernCheckCountRandomLeft(CurrentPlayer) +1
		End If
	Else
	End If
End Sub

Sub WickedCavernCheckRight_Timer()
	WickedCavernCheckRight.Enabled=False
	If WickedCavernCheckCountRandomRight(CurrentPlayer) = 0 Then 
		RandomR01(CurrentPlayer) = randomRight
		WickedCavernCheckCountRandomRight(CurrentPlayer) = WickedCavernCheckCountRandomRight(CurrentPlayer) +1
		CheckLamp.Enabled = True
	ElseIf WickedCavernCheckCountRandomRight(CurrentPlayer) = 1 Then 
		If RandomR01(CurrentPlayer) = randomRight Then
			WickedCavernRandomRightLight
		Else
			RandomR02(CurrentPlayer) = randomRight 
			WickedCavernCheckCountRandomRight(CurrentPlayer) = WickedCavernCheckCountRandomRight(CurrentPlayer) +1
		End If
	ElseIf WickedCavernCheckCountRandomRight(CurrentPlayer) = 2 Then 
		If RandomR01(CurrentPlayer) = randomRight Or RandomR02(CurrentPlayer) = randomRight Then
			WickedCavernRandomRightLight
		Else
			RandomR03(CurrentPlayer) = randomRight 
			WickedCavernCheckCountRandomRight(CurrentPlayer) = WickedCavernCheckCountRandomRight(CurrentPlayer) +1
		End If
	Else
	End If
End Sub

Sub WickedCavernRandomLeftLight
		Select Case Int(Rnd*4)+1
			Case 1 : randomLeft = "L66":'
			Case 2 : randomLeft = "L72":'
			Case 3 : randomLeft = "L79":'
			Case 4 : randomLeft = "L85":'
		End Select
	WickedCavernCheckLeft.Enabled=True
End Sub

Sub WickedCavernRandomRightLight
		Select Case Int(Rnd*4)+1
			Case 1 : randomRight = "L85":'
			Case 2 : randomRight = "L91":'
			Case 3 : randomRight = "L98":'
			Case 4 : randomRight = "L103":'
		End Select
	WickedCavernCheckRight.Enabled=True
End Sub

Sub CheckLamp_timer()
	If MudBog(CurrentPlayer) <> 2 And MoltenFire(CurrentPlayer) <> 2 And BurningSands(CurrentPlayer) <> 2 And WickedCavern(CurrentPlayer) <> 2 And DeepFreeze(CurrentPlayer) <> 2 And BlackCastle(CurrentPlayer) <> 2 And CurrentMissionFlag(CurrentPlayer) = 1 Then
		CurrentMissionFlag(CurrentPlayer) = 0			'When The Current mission is not return to zero > Check and if Bad force to Zero
	End If

	If MysteryFlag = True Then
		L96.State = 2
	Else
		L96.State = 0
	End If	
	'******** Ball Saver & ExtraBall ***********
	If bBallSaverActive = True Then
		L14.BlinkInterval = 160
		L14.State = 2
	ElseIf ExtraBallsAwards(CurrentPlayer) = 1 Then
		L14.State = 1
	Else 
		L14.State = 0
	End If
	'******** ExtraBall Is Lit ***********
	If ExtraBallsIsLit = True Then
		L95.State = 1
	Else
		L95.State = 0
	End If
	'******** Super Is Lit ***********
	If SuperIsLit = True Then
		L88.State = 1
	Else
		L88.State = 0
	End If

	'--------->'29,35,41,47,53,59
	If L29State = 1 Then
		L29.State = 2
	Else
		L29.State = 0
	End If
	If L35State = 1 Then
		L35.State = 2
	Else
		L35.State = 0
	End If
	If L41State = 1 Then
		L41.State = 2
	Else
		L41.State = 0
	End If
	If L47State = 1 Then
		L47.State = 2
	Else
		L47.State = 0
	End If
	If L53State = 1 Then
		L53.State = 2
	Else
		L53.State = 0
	End If
	If L59State = 1 Then
		L59.State = 2
	Else
		L59.State = 0
	End If



'******************************** Color corresponding to Super Mode *****************************************
		If SuperFeaturesColor = "Yellow" Then
			LightObjectColor(2).color = RGB(255, 255, 0) : LightObjectColor(2).colorfull = RGB(255, 255, 0) : '
			LightObjectColor(4).color = RGB(255, 255, 0) : LightObjectColor(4).colorfull = RGB(255, 255, 0) : '
			LightObjectColor(6).color = RGB(255, 255, 0) : LightObjectColor(6).colorfull = RGB(255, 255, 0) : '
			LightObjectColor(8).color = RGB(255, 255, 0) : LightObjectColor(8).colorfull = RGB(255, 255, 0) : '
			LightObjectColor(10).color = RGB(255, 255, 0) : LightObjectColor(10).colorfull = RGB(255, 255, 0) : '
			LightObjectColor(12).color = RGB(255, 255, 0) : LightObjectColor(12).colorfull = RGB(255, 255, 0) : '
		Elseif SuperFeaturesColor = "Red" Then
			LightObjectColor(2).color = RGB(255, 0, 0) : LightObjectColor(2).colorfull = RGB(255, 0, 0) : '
			LightObjectColor(4).color = RGB(255, 0, 0) : LightObjectColor(4).colorfull = RGB(255, 0, 0) : '
			LightObjectColor(6).color = RGB(255, 0, 0) : LightObjectColor(6).colorfull = RGB(255, 0, 0) : '
			LightObjectColor(8).color = RGB(255, 0, 0) : LightObjectColor(8).colorfull = RGB(255, 0, 0) : '
			LightObjectColor(10).color = RGB(255, 0, 0) : LightObjectColor(10).colorfull = RGB(255, 0, 0) : '
			LightObjectColor(12).color = RGB(255, 0, 0) : LightObjectColor(12).colorfull = RGB(255, 0, 0) : '
		Elseif SuperFeaturesColor = "Magenta" Then
			LightObjectColor(2).color = RGB(255, 0, 255) : LightObjectColor(2).colorfull = RGB(255, 0, 255) : '
			LightObjectColor(4).color = RGB(255, 0, 255) : LightObjectColor(4).colorfull = RGB(255, 0, 255) : '
			LightObjectColor(6).color = RGB(255, 0, 255) : LightObjectColor(6).colorfull = RGB(255, 0, 255) : '
			LightObjectColor(8).color = RGB(255, 0, 255) : LightObjectColor(8).colorfull = RGB(255, 0, 255) : '
			LightObjectColor(10).color = RGB(255, 0, 255) : LightObjectColor(10).colorfull = RGB(255, 0, 255) : '
			LightObjectColor(12).color = RGB(255, 0, 255) : LightObjectColor(12).colorfull = RGB(255, 0, 255) : '
		Elseif SuperFeaturesColor = "Green" Then
			LightObjectColor(2).color = RGB(0, 255, 0) : LightObjectColor(2).colorfull = RGB(0, 255, 0) : '
			LightObjectColor(4).color = RGB(0, 255, 0) : LightObjectColor(4).colorfull = RGB(0, 255, 0) : '
			LightObjectColor(6).color = RGB(0, 255, 0) : LightObjectColor(6).colorfull = RGB(0, 255, 0) : '
			LightObjectColor(8).color = RGB(0, 255, 0) : LightObjectColor(8).colorfull = RGB(0, 255, 0) : '
			LightObjectColor(10).color = RGB(0, 255, 0) : LightObjectColor(10).colorfull = RGB(0, 255, 0) : '
			LightObjectColor(12).color = RGB(0, 255, 0) : LightObjectColor(12).colorfull = RGB(0, 255, 0) : '
		End If
'************************************************************************************************************




	If LightChecked3x = 3 Then
		LightChecked3x = 0
		CheckLamp.Enabled=False
	Else
		CheckLamp.Interval = 250
		LightChecked3x = LightChecked3x + 1
	End If
'******************** Restore speed Lightning ****************************************************************
	If RedColorOnly = False Then
		LightObjectColor(1).color = RGB(0, 255, 0) : LightObjectColor(1).colorfull = RGB(0, 255, 0) : 'MudBog = 1
		LightObjectColor(3).color = RGB(255, 50, 0) : LightObjectColor(3).colorfull = RGB(255, 50, 0) : 'MoltenFire = 1
		LightObjectColor(5).color = RGB(255, 255, 0) : LightObjectColor(5).colorfull = RGB(255, 255, 0): 'BurningSands = 1
		LightObjectColor(7).color = RGB(255, 0, 0) : LightObjectColor(7).colorfull = RGB(255, 0, 0) : 'WickedCavern = 1
		LightObjectColor(9).color = RGB(0, 0, 255) : LightObjectColor(9).colorfull = RGB(0, 0, 255) : 'DeepFreeze = 1
		LightObjectColor(11).color = RGB(255, 0, 255) : LightObjectColor(11).colorfull = RGB(255, 0, 255) : 'BlackCastle = 1
	End If
	LightObjectColor(1).Intensity= 50
	LightObjectColor(2).Intensity= 50
	LightObjectColor(3).Intensity= 50
	LightObjectColor(4).Intensity= 50
	LightObjectColor(5).Intensity= 50
	LightObjectColor(6).Intensity= 50
	LightObjectColor(7).Intensity= 50
	LightObjectColor(8).Intensity= 50
	LightObjectColor(9).Intensity= 50
	LightObjectColor(10).Intensity= 50
	LightObjectColor(11).Intensity= 50
	LightObjectColor(12).Intensity= 50
	LightObjectColor(1).FadeSpeedDown = LightObjectColor(1).Intensity /  10
	LightObjectColor(2).FadeSpeedDown = LightObjectColor(2).Intensity /  10
	LightObjectColor(3).FadeSpeedDown = LightObjectColor(3).Intensity /  10
	LightObjectColor(4).FadeSpeedDown = LightObjectColor(4).Intensity /  10
	LightObjectColor(5).FadeSpeedDown = LightObjectColor(5).Intensity /  10
	LightObjectColor(6).FadeSpeedDown = LightObjectColor(6).Intensity /  10
	LightObjectColor(7).FadeSpeedDown = LightObjectColor(7).Intensity /  10
	LightObjectColor(8).FadeSpeedDown = LightObjectColor(8).Intensity /  10
	LightObjectColor(9).FadeSpeedDown = LightObjectColor(9).Intensity /  10
	LightObjectColor(10).FadeSpeedDown = LightObjectColor(10).Intensity /  10
	LightObjectColor(11).FadeSpeedDown = LightObjectColor(11).Intensity /  10
	LightObjectColor(12).FadeSpeedDown = LightObjectColor(12).Intensity /  10
'******************** Knight lock lit ***********************************************************************

	If Knight_challenge(CurrentPlayer) = 0 Then L101.State=2:L94.State=0: End If
	If Knight_challenge(CurrentPlayer) = 1 Then L94.State = 2:L101.State=0:End If
	If Knight_challenge(CurrentPlayer) = 2 Then L101.State = 2:L94.State=0:End If
	If Knight_challenge(CurrentPlayer) = 3 Then L94.State = 2:L101.State=0:End If
	If Knight_challenge(CurrentPlayer) = 4 Then L101.State = 2:L94.State=0:End If
	If Knight_challenge(CurrentPlayer) = 5 Then L94.State = 2:L101.State=0:End If
	If Knight_challenge(CurrentPlayer) = 6 Then L101.State = 2:L94.State=0:End If


'*****************************************************************************************************
	RemainingHit(CurrentPlayer) = NumberOfMissioncomplete(CurrentPlayer) - NumberOfHitForStartMission(CurrentPlayer)
	If RemainingHit(CurrentPlayer) = 0 Or RemainingHit(CurrentPlayer) = 4 Then ' RED
		L79.color = RGB(255, 0, 0)
		L79.colorfull = RGB(255, 0, 0)
		L85.color = RGB(255, 0, 0)
		L85.colorfull = RGB(255, 0, 0)
		L91.color = RGB(255, 0, 0)
		L91.colorfull = RGB(255, 0, 0)
	End If
	If RemainingHit(CurrentPlayer) = 1 Or RemainingHit(CurrentPlayer) = 5 Then	' ORANGE
		L79.color = RGB(255, 50, 0)
		L79.colorfull = RGB(255, 50, 0)
		L85.color = RGB(255, 50, 0)
		L85.colorfull = RGB(255, 50, 0)
		L91.color = RGB(255, 50, 0)
		L91.colorfull = RGB(255, 50, 0)		
	End If
	If RemainingHit(CurrentPlayer) = 2 Or RemainingHit(CurrentPlayer) = 6 Then	' YELLOW
		L79.color = RGB(255, 255, 0)
		L79.colorfull = RGB(255, 255, 0)
		L85.color = RGB(255, 255, 0)
		L85.colorfull = RGB(255, 255, 0)
		L91.color = RGB(255, 255, 0)
		L91.colorfull = RGB(255, 255, 0)
	End If
	If RemainingHit(CurrentPlayer) = 3 Or RemainingHit(CurrentPlayer) = 7 Then	' GRREN
		L79.color = RGB(0, 255, 0)
		L79.colorfull = RGB(0, 255, 0)
		L85.color = RGB(0, 255, 0)
		L85.colorfull = RGB(0, 255, 0)
		L91.color = RGB(0, 255, 0)
		L91.colorfull = RGB(0, 255, 0)
	End If


	If LightsequenceAnimation.Enabled = False Then
		If RampChangeStatus(CurrentPlayer) <>  RampLightsMove(CurrentPlayer) Then
			RampLightsBlinkInterval = 125
			RampLightsState = 2
			If RampLightsMove(CurrentPlayer) = "Blinking" Then
				Light055.BlinkPattern="10"
				Light059.BlinkPattern="10"
				Light054.BlinkPattern="10"
				Light060.BlinkPattern="10"
				Light056.BlinkPattern="10"
				Light061.BlinkPattern="10"
				Light057.BlinkPattern="10"
				Light058.BlinkPattern="10"
				Light062.BlinkPattern="10"
				Light063.BlinkPattern="10"
				RampLightsBlinkInterval = 125
				RampLightsState = 2
				L85.BlinkPattern="10"
				L91.BlinkPattern="10"
				L79.BlinkPattern="10"
				L85.State = 2
				L91.State = 2
				L79.State = 2
				L83.State = 0
				L82.State = 0
				L66.State = 0
				L72.State = 0
				'L79.State = 0
				'L85.State = 0
				'L91.State = 0
				L98.State = 0
				L103.State = 0

				L85.BlinkInterval=125
				L91.BlinkInterval=125
				L79.BlinkInterval=125
				RampChangeStatus(CurrentPlayer) = "Blinking"
			End If

			If RampLightsMove(CurrentPlayer) = "UP" Then
				L79.color = RGB(255, 0, 0)
				L79.colorfull = RGB(255, 0, 0)
				L85.color = RGB(255, 0, 0)
				L85.colorfull = RGB(255, 0, 0)
				L91.color = RGB(255, 0, 0)
				L91.colorfull = RGB(255, 0, 0)
				Light055.BlinkPattern="00010000"
				Light059.BlinkPattern="00010000"
				Light054.BlinkPattern="00001000"
				Light060.BlinkPattern="00001000"
				Light056.BlinkPattern="00000100"
				Light061.BlinkPattern="00000100"
				Light057.BlinkPattern="00000010"
				Light058.BlinkPattern="00000010"
				Light062.BlinkPattern="00000001"
				Light063.BlinkPattern="00000001"
				L79.color = RGB(18, 0, 0)
				L79.colorfull = RGB(255, 0, 0)
				RampLightsBlinkInterval = 125
				RampLightsState = 2
				L83.BlinkPattern="10000000"
				L82.BlinkPattern="01000000"
				L79.BlinkPattern="00100000"
				L83.State = 2
				L82.State = 2
				L79.State = 2
				L85.State = 0
				L91.State = 0
				L66.State = 0
				L72.State = 0
				L98.State = 0
				L103.State = 0
				L83.BlinkInterval=125
				L82.BlinkInterval=125
				L79.BlinkInterval=125
				RampChangeStatus(CurrentPlayer) = "UP"
			End If
			If RampLightsMove(CurrentPlayer) = "DOWN" Then
				Light055.BlinkPattern="00001"
				Light059.BlinkPattern="00001"
				Light054.BlinkPattern="00010"
				Light060.BlinkPattern="00010"
				Light056.BlinkPattern="00100"
				Light061.BlinkPattern="00100"
				Light057.BlinkPattern="01000"
				Light058.BlinkPattern="01000"
				Light062.BlinkPattern="10000"
				Light063.BlinkPattern="10000"
				RampLightsBlinkInterval = 125
				RampLightsState = 2
				RampChangeStatus(CurrentPlayer) = "Down"
			End If
			If RampLightsMove(CurrentPlayer) = "OFF" Then
				RampLightsState = 0
				RampChangeStatus(CurrentPlayer) = "OFF"
			End If
			If RampLightsMove(CurrentPlayer) = "ShieldReadyForMonster" Then
				Light055.BlinkPattern="10"
				Light059.BlinkPattern="10"
				Light054.BlinkPattern="10"
				Light060.BlinkPattern="10"
				Light056.BlinkPattern="10"
				Light061.BlinkPattern="10"
				Light057.BlinkPattern="10"
				Light058.BlinkPattern="10"
				Light062.BlinkPattern="10"
				Light063.BlinkPattern="10"
				RampLightsBlinkInterval = 125
				RampLightsState = 2
				L91.color = RGB(18, 0, 0)
				L91.colorfull = RGB(255, 0, 0)
				If MissionRandom = "MudBog" Then 
					L91.color = RGB(0, 255, 0)
					L91.colorfull = RGB(0, 255, 0)
				End If
				If MissionRandom = "MoltenFire" Then
					L91.color = RGB(255, 128, 0)
					L91.colorfull = RGB(255, 128, 0)
				End If
				If MissionRandom = "BurningSands" Then
					L91.color = RGB(255, 255, 0)
					L91.colorfull = RGB(255, 255, 0)
				End If
				If MissionRandom = "WickedCavern" Then
					L91.color = RGB(255, 0, 0)
					L91.colorfull = RGB(255, 0, 0)
				End If
				If MissionRandom = "DeepFreeze" Then
					L91.color = RGB(0, 0, 255)
					L91.colorfull = RGB(0, 0, 255)
				End If
				If MissionRandom = "BlackCastle" Then
					L91.color = RGB(255, 0, 255)
					L91.colorfull = RGB(255, 0, 255)
				End If
				L83.State = 0
				L82.State = 0
				L79.State = 0
				L85.State = 0
				L91.State = 2
				L66.State = 0
				L72.State = 0
				L98.State = 0
				L103.State = 0
				
				L91.BlinkInterval=125
				RampChangeStatus(CurrentPlayer) = "ShieldReadyForMonster"
			End If
			On Error Resume Next
			Dim r
			For each r in RampLights
				r.State = RampLightsState
				r.BlinkInterval=RampLightsBlinkInterval
			Next
		End If

'End Sub
If RedColorOnly = False Then
	'dim LightObjectGreen(12)
	LightObjectColor(1).color = RGB(0, 255, 0) : LightObjectColor(1).colorfull = RGB(0, 255, 0) : 'MudBog = 1
	LightObjectColor(3).color = RGB(255, 50, 0) : LightObjectColor(3).colorfull = RGB(255, 50, 0) : 'MoltenFire = 1
	LightObjectColor(5).color = RGB(255, 255, 0) : LightObjectColor(5).colorfull = RGB(255, 255, 0): 'BurningSands = 1
	LightObjectColor(7).color = RGB(255, 0, 0) : LightObjectColor(7).colorfull = RGB(255, 0, 0) : 'WickedCavern = 1
	LightObjectColor(9).color = RGB(0, 0, 255) : LightObjectColor(9).colorfull = RGB(0, 0, 255) : 'DeepFreeze = 1
	LightObjectColor(11).color = RGB(255, 0, 255) : LightObjectColor(11).colorfull = RGB(255, 0, 255) : 'BlackCastle = 1
End If



		If MudBog(CurrentPlayer) = 1 Then LightObjectColor(1).state=2:LightObjectColor(1).BlinkPattern=10:LightObjectColor(1).BlinkInterval=125 End If	
		If MudBog(CurrentPlayer) = 2 Then LightObjectColor(1).state=2:LightObjectColor(1).BlinkPattern =10:LightObjectColor(1).BlinkInterval=50 End If
		If MudBog(CurrentPlayer) = 3 Then 
			If LightObjectColor(1).state<>2 Then LightObjectColor(1).state=2:LightObjectColor(1).BlinkPattern =1111100:LightObjectColor(1).BlinkInterval=200 End If
		End If
		If MudBog(CurrentPlayer) = 4 Then LightObjectColor(1).state=1 End If

		If MoltenFire(CurrentPlayer) = 1 Then LightObjectColor(3).state=2:LightObjectColor(3).BlinkPattern=10:LightObjectColor(3).BlinkInterval=125 End If
		If MoltenFire(CurrentPlayer) = 2 Then LightObjectColor(3).state=2:LightObjectColor(3).BlinkPattern =10:LightObjectColor(3).BlinkInterval=50 End If
		If MoltenFire(CurrentPlayer) = 3 Then 
			If LightObjectColor(3).state<>2 Then LightObjectColor(3).state=2:LightObjectColor(3).BlinkPattern =1111100:LightObjectColor(3).BlinkInterval=200 End If
		End If 
		If MoltenFire(CurrentPlayer) = 4 Then LightObjectColor(3).state=1 End If

		If BurningSands(CurrentPlayer) = 1 Then LightObjectColor(5).state=2:LightObjectColor(5).BlinkPattern=10:LightObjectColor(5).BlinkInterval=125 End If
		If BurningSands(CurrentPlayer) = 2 Then LightObjectColor(5).state=2:LightObjectColor(5).BlinkPattern=10:LightObjectColor(5).BlinkInterval=50 End If
		If BurningSands(CurrentPlayer) = 3 Then 
			If LightObjectColor(5).state<>2 Then LightObjectColor(5).state=2:LightObjectColor(5).BlinkPattern=1111100:LightObjectColor(5).BlinkInterval=200 End If
		End If
		If BurningSands(CurrentPlayer) = 4 Then LightObjectColor(5).state=1 End If

		If WickedCavern(CurrentPlayer) = 1 Then LightObjectColor(7).state=2:LightObjectColor(7).BlinkPattern=10:LightObjectColor(7).BlinkInterval=125 End If
		If WickedCavern(CurrentPlayer) = 2 Then LightObjectColor(7).state=2:LightObjectColor(7).BlinkPattern=10:LightObjectColor(7).BlinkInterval=50 End If
		If WickedCavern(CurrentPlayer) = 3 Then 
			If LightObjectColor(7).state<>2 Then LightObjectColor(7).state=2:LightObjectColor(7).BlinkPattern=1111100:LightObjectColor(7).BlinkInterval=200 End If
		End If
		If WickedCavern(CurrentPlayer) = 4 Then LightObjectColor(7).state=1 End If

		If DeepFreeze(CurrentPlayer) = 1 Then LightObjectColor(9).state=2:LightObjectColor(9).BlinkPattern=10:LightObjectColor(9).BlinkInterval=125 End If
		If DeepFreeze(CurrentPlayer) = 2 Then LightObjectColor(9).state=2:LightObjectColor(9).BlinkPattern=10:LightObjectColor(9).BlinkInterval=50 End If
		If DeepFreeze(CurrentPlayer) = 3 Then 
			If LightObjectColor(9).state<>2 Then LightObjectColor(9).state=2:LightObjectColor(9).BlinkPattern=1111100:LightObjectColor(9).BlinkInterval=200  End If
		End If 
		If DeepFreeze(CurrentPlayer) = 4 Then LightObjectColor(9).state=1 End If

		If BlackCastle(CurrentPlayer) = 1 Then LightObjectColor(11).state=2:LightObjectColor(11).BlinkPattern=10:LightObjectColor(11).BlinkInterval=125 End If
		If BlackCastle(CurrentPlayer) = 2 Then LightObjectColor(11).state=2:LightObjectColor(11).BlinkPattern=10:LightObjectColor(11).BlinkInterval=50 End If
		If BlackCastle(CurrentPlayer) = 3 Then 
			If LightObjectColor(11).state<>2 Then LightObjectColor(11).state=2:LightObjectColor(11).BlinkPattern=1111100:LightObjectColor(11).BlinkInterval=200  End If
		End If 
		If BlackCastle(CurrentPlayer) = 4 Then LightObjectColor(11).state=1 End If




	End If
	If KnightLamp(CurrentPlayer) = 0 Then L18.state=0 : L19.state=0 : L20.state=0 : L21.state=0 : L22.state=0 : L23.state=0 : End If		'Nothing
	If KnightLamp(CurrentPlayer) = 2 Then L18.state=1 : L19.state=0 : L20.state=0 : L21.state=0 : L22.state=0 : L23.state=0: End If		'K
	If KnightLamp(CurrentPlayer) = 4 Then L18.state=1 : L19.state=1 : L20.state=0 : L21.state=0 : L22.state=0 : L23.state=0: End If		'N
	If KnightLamp(CurrentPlayer) = 6 Then L18.state=1 : L19.state=1 : L20.state=1 : L21.state=0 : L22.state=0 : L23.state=0: End If		'I
	If KnightLamp(CurrentPlayer) = 8 Then L18.state=1 : L19.state=1 : L20.state=1 : L21.state=1 : L22.state=0 : L23.state=0: End If		'G
	If KnightLamp(CurrentPlayer) = 10 Then L18.state=1 : L19.state=1 : L20.state=1 : L21.state=1 : L22.state=1 : L23.state=0: End If	'H
	If KnightLamp(CurrentPlayer) = 12 Then L18.state=1 : L19.state=1 : L20.state=1 : L21.state=1 : L22.state=1 : L23.state=1: End If	'T
	'If KnightLamp = 7 Then L18.state=1 : L19.state=1 : L20.state=1 : L21.state=1 : L22.state=1 : L23.state=1: End If
	
	If catapult_lock_is_lit(CurrentPlayer) = True AND CatapultModeFlag = False Then
		If BallInCatapult(CurrentPlayer) = 0 Then
			L131.state = 2
			L132.state = 0
			L133.state = 0
		End If
		If BallInCatapult(CurrentPlayer) = 1 Then
			L131.state = 1
			L132.state = 2
			L133.state = 0		
		End If
		If BallInCatapult(CurrentPlayer) = 2 Then
			L131.state = 1
			L132.state = 1
			L133.state = 2		
		End If
		If BallInCatapult(CurrentPlayer) = 3 Then
			L131.state = 1
			L132.state = 1
			L133.state = 1		
		End If
	Elseif CatapultModeFlag = False Then
		L131.state = 0
		L132.state = 0
		L133.state = 0
	End If

	If MudBog(CurrentPlayer) = 2 Then 				'*********************** Set Light Color for MudBog *******************************************************
		L66.color = RGB(0, 255, 0)
		L66.colorfull = RGB(0, 255, 0)
		L72.color = RGB(0, 255, 0)
		L72.colorfull = RGB(0, 255, 0)
		L79.color = RGB(0, 255, 0)
		L79.colorfull = RGB(0, 255, 0)
		L98.color = RGB(0, 255, 0)
		L98.colorfull = RGB(0, 255, 0)
		L103.color = RGB(0, 255, 0)
		L103.colorfull = RGB(0, 255, 0)
		L66color(CurrentPlayer) = "green"
		L72color(CurrentPlayer) = "green"
		L79color(CurrentPlayer) = "green"
		L85color(CurrentPlayer) = "green"
		L91color(CurrentPlayer) = "green"
		L98color(CurrentPlayer) = "green"
		L103color(CurrentPlayer)= "green"
		
		If MudBogDefeated(CurrentPlayer) = 0 Then					'*********************** Start Mission  - Set Light for MudBog ********************************
			L66State(CurrentPlayer) = 2
			L72State(CurrentPlayer) = 2
			L79State(CurrentPlayer) = 2
			L85State(CurrentPlayer) = 0
			L91State(CurrentPlayer) = 0
			L98State(CurrentPlayer) = 2
			L103State(CurrentPlayer) = 2
		End If
	End If

	If MoltenFire(CurrentPlayer) = 2 Then 			'*********************** Set Light Color for MoltenFire *******************************************************
		L66.color = RGB(255, 50, 0)
		L66.colorfull = RGB(255, 50, 0)
		L72.color = RGB(255, 50, 0)
		L72.colorfull = RGB(255, 50, 0)
		L79.color = RGB(255, 50, 0)
		L79.colorfull = RGB(255, 50, 0)
		L85.color = RGB(255, 50, 0)
		L85.colorfull = RGB(255, 50, 0)
		L91.color = RGB(255, 50, 0)
		L91.colorfull = RGB(255, 50, 0)
		L98.color = RGB(255, 50, 0)
		L98.colorfull = RGB(255, 50, 0)
		L103.color = RGB(255, 50, 0)
		L103.colorfull = RGB(255, 50, 0)
		L66color(CurrentPlayer) = "orange"
		L72color(CurrentPlayer) = "orange"
		L79color(CurrentPlayer) = "orange"
		L85color(CurrentPlayer) = "orange"
		L91color(CurrentPlayer) = "orange"
		L98color(CurrentPlayer) = "orange"
		L103color(CurrentPlayer)= "orange"
		If MoltenFireDefeated(CurrentPlayer) = 0 Then				'*********************** Start Mission  - Set Light for MoltenFire ********************************
			L66State(CurrentPlayer) = 2
			L72State(CurrentPlayer) = 2
			L79State(CurrentPlayer) = 2
			L85State(CurrentPlayer) = 2
			L91State(CurrentPlayer) = 2
			L98State(CurrentPlayer) = 2
			L103State(CurrentPlayer) = 2	
		End If
	End If
	If BurningSands(CurrentPlayer) = 2 Then 	'*********************** Set Light Color for BurningSands *******************************************************
		L66.color = RGB(255, 255, 0)
		L66.colorfull = RGB(255, 255, 0)
		L72.color = RGB(255, 255, 0)
		L72.colorfull = RGB(255, 255, 0)
		L79.color = RGB(255, 255, 0)
		L79.colorfull = RGB(255, 255, 0)
		L85.color = RGB(255, 255, 0)
		L85.colorfull = RGB(255, 255, 0)
		L91.color = RGB(255, 255, 0)
		L91.colorfull = RGB(255, 255, 0)
		L98.color = RGB(255, 255, 0)
		L98.colorfull = RGB(255, 255, 0)
		L103.color = RGB(255, 255, 0)
		L103.colorfull = RGB(255, 255, 0)
		L66color(CurrentPlayer) = "yellow"
		L72color(CurrentPlayer) = "yellow"
		L79color(CurrentPlayer) = "yellow"
		L85color(CurrentPlayer) = "yellow"
		L91color(CurrentPlayer) = "yellow"
		L98color(CurrentPlayer) = "yellow"
		L103color(CurrentPlayer)= "yellow"

		If BurningSandsLightMovementNumber = 1 And BurningSandsHit = 0 Then		'*********************** Start Light Animation Mission  - Set Light for BurningSands ********************************
			L66State(CurrentPlayer) = 2
			L72State(CurrentPlayer) = 2
			L79State(CurrentPlayer) = 2
			L85State(CurrentPlayer) = 0
			L91State(CurrentPlayer) = 0
			L98State(CurrentPlayer) = 0
			L103State(CurrentPlayer) = 0
		End If
		If BurningSandsLightMovementNumber = 2 And BurningSandsHit = 0 Then
			L66State(CurrentPlayer) = 0
			L72State(CurrentPlayer) = 2
			L79State(CurrentPlayer) = 2
			L85State(CurrentPlayer) = 2
			L91State(CurrentPlayer) = 0
			L98State(CurrentPlayer) = 0
			L103State(CurrentPlayer) = 0
		End If
		If BurningSandsLightMovementNumber = 3 And BurningSandsHit = 0 Then
			L66State(CurrentPlayer) = 0
			L72State(CurrentPlayer) = 0
			L79State(CurrentPlayer) = 2
			L85State(CurrentPlayer) = 2
			L91State(CurrentPlayer) = 2
			L98State(CurrentPlayer) = 0
			L103State(CurrentPlayer) = 0
		End If
		If BurningSandsLightMovementNumber = 4 And BurningSandsHit = 0 Then
			L66State(CurrentPlayer) = 0
			L72State(CurrentPlayer) = 0
			L79State(CurrentPlayer) = 0
			L85State(CurrentPlayer) = 2
			L91State(CurrentPlayer) = 2
			L98State(CurrentPlayer) = 2
			L103State(CurrentPlayer) = 0
		End If
		If BurningSandsLightMovementNumber = 5 And BurningSandsHit = 0 Then
			L66State(CurrentPlayer) = 0
			L72State(CurrentPlayer) = 0
			L79State(CurrentPlayer) = 0
			L85State(CurrentPlayer) = 0
			L91State(CurrentPlayer) = 2
			L98State(CurrentPlayer) = 2
			L103State(CurrentPlayer) = 2
		End If
		If BurningSandsLightMovementNumber = 6 And BurningSandsHit = 0 Then
			L66State(CurrentPlayer) = 0
			L72State(CurrentPlayer) = 0
			L79State(CurrentPlayer) = 0
			L85State(CurrentPlayer) = 2
			L91State(CurrentPlayer) = 2
			L98State(CurrentPlayer) = 2
			L103State(CurrentPlayer) = 0
		End If
		If BurningSandsLightMovementNumber = 7 And BurningSandsHit = 0 Then
			L66State(CurrentPlayer) = 0
			L72State(CurrentPlayer) = 0
			L79State(CurrentPlayer) = 2
			L85State(CurrentPlayer) = 2
			L91State(CurrentPlayer) = 2
			L98State(CurrentPlayer) = 0
			L103State(CurrentPlayer) = 0
		End If
		If BurningSandsLightMovementNumber = 8 And BurningSandsHit = 0 Then
			L66State(CurrentPlayer) = 0
			L72State(CurrentPlayer) = 2
			L79State(CurrentPlayer) = 2
			L85State(CurrentPlayer) = 2
			L91State(CurrentPlayer) = 0
			L98State(CurrentPlayer) = 0
			L103State(CurrentPlayer) = 0
		End If
	End If

	If WickedCavern(CurrentPlayer) = 2 Then	'*********************** Set Light Color for WickedCavern *******************************************************
		L66.color = RGB(255, 0, 0)
		L66.colorfull = RGB(255, 0, 0)
		L72.color = RGB(255, 0, 0)
		L72.colorfull = RGB(255, 0, 0)
		L79.color = RGB(255, 0, 0)
		L79.colorfull = RGB(255, 0, 0)
		L85.color = RGB(255, 0, 0)
		L85.colorfull = RGB(255, 0, 0)
		L91.color = RGB(255, 0, 0)
		L91.colorfull = RGB(255, 0, 0)
		L98.color = RGB(255, 0, 0)
		L98.colorfull = RGB(255, 0, 0)
		L103.color = RGB(255, 0, 0)
		L103.colorfull = RGB(255, 0, 0)
		L66color(CurrentPlayer) = "red"
		L72color(CurrentPlayer) = "red"
		L79color(CurrentPlayer) = "red"
		L85color(CurrentPlayer) = "red"
		L91color(CurrentPlayer) = "red"
		L98color(CurrentPlayer) = "red"
		L103color(CurrentPlayer)= "red"

		'********************** WickedCavern ************************************
		If WickedCavernDefeated(CurrentPlayer) = 0 And RandomL01(CurrentPlayer) = "" Then
			If RandomR01(CurrentPlayer) = "" Then
				MissionHit 0
			End If
		End If
		If WickedCavernLeftOrRightOld = "left" Then
			If RandomL01(CurrentPlayer) = "L66" Or  RandomL02(CurrentPlayer) = "L66" Or RandomL03(CurrentPlayer) = "L66" Then L66State(CurrentPlayer)=2:L91State(CurrentPlayer) = 0:L98State(CurrentPlayer) = 0:L103State(CurrentPlayer) = 0:End If
			If RandomL01(CurrentPlayer) = "L72" Or  RandomL02(CurrentPlayer) = "L72" Or RandomL03(CurrentPlayer) = "L72" Then L72State(CurrentPlayer)=2:L91State(CurrentPlayer) = 0:L98State(CurrentPlayer) = 0:L103State(CurrentPlayer) = 0:End If
			If RandomL01(CurrentPlayer) = "L79" Or  RandomL02(CurrentPlayer) = "L79" Or RandomL03(CurrentPlayer) = "L79" Then L79State(CurrentPlayer)=2:L91State(CurrentPlayer) = 0:L98State(CurrentPlayer) = 0:L103State(CurrentPlayer) = 0:End If
			If RandomL01(CurrentPlayer) = "L85" Or  RandomL02(CurrentPlayer) = "L85" Or RandomL03(CurrentPlayer) = "L85" Then L85State(CurrentPlayer)=2:L91State(CurrentPlayer) = 0:L98State(CurrentPlayer) = 0:L103State(CurrentPlayer) = 0:End If
			If WickedCavernCheckCountRandomLeft(CurrentPlayer) = 4 Then
				L66State(CurrentPlayer) = 2
				L72State(CurrentPlayer) = 2
				L79State(CurrentPlayer) = 2
				L85State(CurrentPlayer) = 2
				L91State(CurrentPlayer) = 0
				L98State(CurrentPlayer) = 0
				L103State(CurrentPlayer) = 0
			End If
		Else
			If RandomR01(CurrentPlayer) = "L85" Or  RandomR02(CurrentPlayer) = "L85" Or RandomR03(CurrentPlayer) = "L85" Then L85State(CurrentPlayer)=2:L66State(CurrentPlayer) = 0:L72State(CurrentPlayer) = 0:L79State(CurrentPlayer) = 0:End If
			If RandomR01(CurrentPlayer) = "L91" Or  RandomR02(CurrentPlayer) = "L91" Or RandomR03(CurrentPlayer) = "L91" Then L91State(CurrentPlayer)=2:L66State(CurrentPlayer) = 0:L72State(CurrentPlayer) = 0:L79State(CurrentPlayer) = 0:End If
			If RandomR01(CurrentPlayer) = "L98" Or  RandomR02(CurrentPlayer) = "L98" Or RandomR03(CurrentPlayer) = "L98" Then L98State(CurrentPlayer)=2:L66State(CurrentPlayer) = 0:L72State(CurrentPlayer) = 0:L79State(CurrentPlayer) = 0:End If
			If RandomR01(CurrentPlayer) = "L103"Or  RandomR02(CurrentPlayer) = "L103"Or RandomR03(CurrentPlayer) = "L103" Then L103State(CurrentPlayer)=2:L66State(CurrentPlayer) = 0:L72State(CurrentPlayer) = 0:L79State(CurrentPlayer) = 0:End If
			If WickedCavernCheckCountRandomRight(CurrentPlayer) = 4 Then
				L66State(CurrentPlayer) = 0
				L72State(CurrentPlayer) = 0
				L79State(CurrentPlayer) = 0
				L85State(CurrentPlayer) = 2
				L91State(CurrentPlayer) = 2
				L98State(CurrentPlayer) = 2
				L103State(CurrentPlayer) = 2
			End If
		End If
	End If		


	If DeepFreeze(CurrentPlayer) = 2 Then	'*********************** Set Light Color for DeepFreeze *******************************************************
		'********************** DeepFreeze ************************************
		If DeepFreezeDefeated(CurrentPlayer) = 0 And DeepFreezeMissionHitStart(CurrentPlayer) = False Then
			L66color(CurrentPlayer) = "blue"
			L72color(CurrentPlayer) = "blue"
			L79color(CurrentPlayer) = "blue"
			L85color(CurrentPlayer) = "blue"
			L91color(CurrentPlayer) = "blue"
			L98color(CurrentPlayer) = "blue"
			L103color(CurrentPlayer)= "blue"
			L66State(CurrentPlayer) = 2
			L72State(CurrentPlayer) = 0
			L79State(CurrentPlayer) = 2
			L85State(CurrentPlayer) = 0
			L91State(CurrentPlayer) = 0
			L98State(CurrentPlayer) = 0
			L103State(CurrentPlayer) = 2
		End If
	End If

	If BlackCastle(CurrentPlayer) = 2 Then	'*********************** Set Light Color for BlackCastle *******************************************************
		'********************** BlackCastle ************************************
		If BlackCastleDefeated(CurrentPlayer) = 0 And BlackCastleMissionHitStart(CurrentPlayer) = False Then
			L66color(CurrentPlayer) = "magenta"
			L72color(CurrentPlayer) = "magenta"
			L79color(CurrentPlayer) = "magenta"
			L85color(CurrentPlayer) = "magenta"
			L91color(CurrentPlayer) = "magenta"
			L98color(CurrentPlayer) = "magenta"
			L103color(CurrentPlayer)= "magenta"
			L66State(CurrentPlayer) = 0
			L72State(CurrentPlayer) = 0
			L79State(CurrentPlayer) = 2
			L85State(CurrentPlayer) = 2
			L91State(CurrentPlayer) = 2
			L98State(CurrentPlayer) = 0
			L103State(CurrentPlayer) = 0
		End If
	End If

	If Knight_challenge_flag = True	Then			'**************************************** Knight Challenge *******************************************
		If Knight_challenge_phase = 0 Then	
			L66color(CurrentPlayer) = "blue"
			L72color(CurrentPlayer) = "blue"
			L79color(CurrentPlayer) = "blue"
			L85color(CurrentPlayer) = "blue"
			L91color(CurrentPlayer) = "blue"
			L98color(CurrentPlayer) = "blue"
			L103color(CurrentPlayer)= "blue"
			L66State(CurrentPlayer) = 2
			L72State(CurrentPlayer) = 0
			L79State(CurrentPlayer) = 2
			L85State(CurrentPlayer) = 0
			L91State(CurrentPlayer) = 2
			L98State(CurrentPlayer) = 0
			L103State(CurrentPlayer) = 0
			Knight_challenge_phase = 1
			video_knight_challenge_background KnightRemaining
		End If
	End If

	If CatapultModeFlag = True Then 				'**************************************** Catapult Multiball *******************************************
		L66color(CurrentPlayer) = "orange"
		L72color(CurrentPlayer) = "orange"
		L79color(CurrentPlayer) = "orange"
		L85color(CurrentPlayer) = "orange"
		L91color(CurrentPlayer) = "orange"
		L98color(CurrentPlayer) = "orange"
		L103color(CurrentPlayer)= "orange"
													
		L66State(CurrentPlayer) = 2  							'L66    (SW41 + SW45) 	-> Left spinner lane
		L72State(CurrentPlayer) = 2  							'L72					-> Left orbit
		L79State(CurrentPlayer) = 2 							'L79					-> Left ramp
		L85State(CurrentPlayer) = 2  							'L85					-> The Black Knight
		L91State(CurrentPlayer) = 2 				 			'L91	(SW58 + SW60)	-> Shield and rear of shield(saucer)
		L98State(CurrentPlayer) = 2  							'L98					-> Light Lock target 
		L103State(CurrentPlayer) = 2 							'L103					-> Right orbit
	End If	

	If WarHurryFlag = True 	Then					'**************************************** War Hurry UP *******************************************
		'L66color(CurrentPlayer) = "orange"
		'L72color(CurrentPlayer) = "orange"
		L79color(CurrentPlayer) = "white"
		L85color(CurrentPlayer) = "white"
		L91color(CurrentPlayer) = "white"
		'L98color(CurrentPlayer) = "orange"
		'L103color(CurrentPlayer)= "orange"
		L79State(CurrentPlayer) = 2 							'L79					-> Left ramp
		L85State(CurrentPlayer) = 2  							'L85					-> The Black Knight
		L91State(CurrentPlayer) = 2 				 			'L91	(SW58 + SW60)	-> Shield and rear of shield(saucer)
	End If

If CurrentMissionFlag(CurrentPlayer) = 1 Or CatapultModeFlag = True Or Knight_challenge_flag = True or WarHurryFlag = True Then
	If L66State(CurrentPlayer) = 0 Then L66.State = 0 End If
	If L72State(CurrentPlayer) = 0 Then L72.State = 0 End If
	If L79State(CurrentPlayer) = 0 Then L79.State = 0 End If
	If L85State(CurrentPlayer) = 0 Then L85.State = 0 End If
	If L91State(CurrentPlayer) = 0 Then L91.State = 0 End If
	If L98State(CurrentPlayer) = 0 Then L98.State = 0 End If
	If L103State(CurrentPlayer) = 0 Then L103.State = 0 End If

	If L66State(CurrentPlayer) = 1 Then L66.State = 1 End If
	If L72State(CurrentPlayer) = 1 Then L72.State = 1 End If
	If L79State(CurrentPlayer) = 1 Then L79.State = 1 End If
	If L85State(CurrentPlayer) = 1 Then L85.State = 1 End If
	If L91State(CurrentPlayer) = 1 Then L91.State = 1 End If
	If L98State(CurrentPlayer) = 1 Then L98.State = 1 End If
	If L103State(CurrentPlayer) = 1 Then L103.State = 1 End If

	If L66State(CurrentPlayer) = 2 Then L66.State = 2 End If
	If L72State(CurrentPlayer) = 2 Then L72.State = 2 End If
	If L79State(CurrentPlayer) = 2 Then L79.State = 2 End If
	If L85State(CurrentPlayer) = 2 Then L85.State = 2 End If
	If L91State(CurrentPlayer) = 2 Then L91.State = 2 End If
	If L98State(CurrentPlayer) = 2 Then L98.State = 2 End If
	If L103State(CurrentPlayer) = 2 Then L103.State = 2 End If

	If L66color(CurrentPlayer) = "red"	Then L66.color = RGB(255, 0, 0):L66.colorfull = RGB(255, 0, 0):End If
	If L72color(CurrentPlayer) = "red"	Then L72.color = RGB(255, 0, 0):L72.colorfull = RGB(255, 0, 0):End If
	If L79color(CurrentPlayer) = "red"	Then L79.color = RGB(255, 0, 0):L79.colorfull = RGB(255, 0, 0):End If
	If L85color(CurrentPlayer) = "red"	Then L85.color = RGB(255, 0, 0):L85.colorfull = RGB(255, 0, 0):End If
	If L91color(CurrentPlayer) = "red"	Then L91.color = RGB(255, 0, 0):L91.colorfull = RGB(255, 0, 0):End If
	If L98color(CurrentPlayer) = "red"	Then L98.color = RGB(255, 0, 0):L98.colorfull = RGB(255, 0, 0):End If
	If L103color(CurrentPlayer)= "red"	Then L103.color = RGB(255, 0, 0):L103.colorfull = RGB(255, 0, 0):End If

	If L66color(CurrentPlayer) = "green" 	Then L66.color = RGB(0, 255, 0):L66.colorfull = RGB(0, 255, 0):End If
	If L72color(CurrentPlayer) = "green"	Then L72.color = RGB(0, 255, 0):L72.colorfull = RGB(0, 255, 0):End If
	If L79color(CurrentPlayer) = "green"	Then L79.color = RGB(0, 255, 0):L79.colorfull = RGB(0, 255, 0):End If
	If L85color(CurrentPlayer) = "green"	Then L85.color = RGB(0, 255, 0):L85.colorfull = RGB(0, 255, 0):End If
	If L91color(CurrentPlayer) = "green"	Then L91.color = RGB(0, 255, 0):L91.colorfull = RGB(0, 255, 0):End If
	If L98color(CurrentPlayer) = "green"	Then L98.color = RGB(0, 255, 0):L98.colorfull = RGB(0, 255, 0):End If
	If L103color(CurrentPlayer)= "green"	Then L103.color = RGB(0, 255, 0):L103.colorfull = RGB(0, 255, 0):End If

	If L66color(CurrentPlayer) = "blue" Then L66.color = RGB(0, 0, 255):L66.colorfull = RGB(0, 0, 255):End If
	If L72color(CurrentPlayer) = "blue"	Then L72.color = RGB(0, 0, 255):L72.colorfull = RGB(0, 0, 255):End If
	If L79color(CurrentPlayer) = "blue"	Then L79.color = RGB(0, 0, 255):L79.colorfull = RGB(0, 0, 255):End If
	If L85color(CurrentPlayer) = "blue"	Then L85.color = RGB(0, 0, 255):L85.colorfull = RGB(0, 0, 255):End If
	If L91color(CurrentPlayer) = "blue"	Then L91.color = RGB(0, 0, 255):L91.colorfull = RGB(0, 0, 255):End If
	If L98color(CurrentPlayer) = "blue"	Then L98.color = RGB(0, 0, 255):L98.colorfull = RGB(0, 0, 255):End If
	If L103color(CurrentPlayer)= "blue"	Then L103.color = RGB(0, 0, 255):L103.colorfull = RGB(0, 0, 255):End If

	If L66color(CurrentPlayer) = "yellow" 	Then L66.color = RGB(255, 255, 0):L66.colorfull = RGB(255, 255, 0):End If
	If L72color(CurrentPlayer) = "yellow"	Then L72.color = RGB(255, 255, 0):L72.colorfull = RGB(255, 255, 0):End If
	If L79color(CurrentPlayer) = "yellow"	Then L79.color = RGB(255, 255, 0):L79.colorfull = RGB(255, 255, 0):End If
	If L85color(CurrentPlayer) = "yellow"	Then L85.color = RGB(255, 255, 0):L85.colorfull = RGB(255, 255, 0):End If
	If L91color(CurrentPlayer) = "yellow"	Then L91.color = RGB(255, 255, 0):L91.colorfull = RGB(255, 255, 0):End If
	If L98color(CurrentPlayer) = "yellow"	Then L98.color = RGB(255, 255, 0):L98.colorfull = RGB(255, 255, 0):End If
	If L103color(CurrentPlayer)= "yellow"	Then L103.color = RGB(255, 255, 0):L103.colorfull = RGB(255, 255, 0):End If

	If L66color(CurrentPlayer) = "orange" 	Then L66.color = RGB(255, 50, 0):L66.colorfull = RGB(255, 50, 0):End If
	If L72color(CurrentPlayer) = "orange"	Then L72.color = RGB(255, 50, 0):L72.colorfull = RGB(255, 50, 0):End If
	If L79color(CurrentPlayer) = "orange"	Then L79.color = RGB(255, 50, 0):L79.colorfull = RGB(255, 50, 0):End If
	If L85color(CurrentPlayer) = "orange"	Then L85.color = RGB(255, 50, 0):L85.colorfull = RGB(255, 50, 0):End If
	If L91color(CurrentPlayer) = "orange"	Then L91.color = RGB(255, 50, 0):L91.colorfull = RGB(255, 50, 0):End If
	If L98color(CurrentPlayer) = "orange"	Then L98.color = RGB(255, 50, 0):L98.colorfull = RGB(255, 50, 0):End If
	If L103color(CurrentPlayer)= "orange"	Then L103.color = RGB(255, 50, 0):L103.colorfull = RGB(255, 50, 0):End If

	If L66color(CurrentPlayer) = "white" 	Then L66.color = RGB(255, 255, 255):L66.colorfull = RGB(255, 255, 255):End If
	If L72color(CurrentPlayer) = "white"	Then L72.color = RGB(255, 255, 255):L72.colorfull = RGB(255, 255, 255):End If
	If L79color(CurrentPlayer) = "white"	Then L79.color = RGB(255, 255, 255):L79.colorfull = RGB(255, 255, 255):End If
	If L85color(CurrentPlayer) = "white"	Then L85.color = RGB(255, 255, 255):L85.colorfull = RGB(255, 255, 255):End If
	If L91color(CurrentPlayer) = "white"	Then L91.color = RGB(255, 255, 255):L91.colorfull = RGB(255, 255, 255):End If
	If L98color(CurrentPlayer) = "white"	Then L98.color = RGB(255, 255, 255):L98.colorfull = RGB(255, 255, 255):End If
	If L103color(CurrentPlayer)= "white"	Then L103.color = RGB(255, 255, 255):L103.colorfull = RGB(255, 255, 255):End If

	If L66color(CurrentPlayer) = "magenta" 	Then L66.color = RGB(255, 0, 255):L66.colorfull = RGB(255, 0, 255):End If
	If L72color(CurrentPlayer) = "magenta"	Then L72.color = RGB(255, 0, 255):L72.colorfull = RGB(255, 0, 255):End If
	If L79color(CurrentPlayer) = "magenta"	Then L79.color = RGB(255, 0, 255):L79.colorfull = RGB(255, 0, 255):End If
	If L85color(CurrentPlayer) = "magenta"	Then L85.color = RGB(255, 0, 255):L85.colorfull = RGB(255, 0, 255):End If
	If L91color(CurrentPlayer) = "magenta"	Then L91.color = RGB(255, 0, 255):L91.colorfull = RGB(255, 0, 255):End If
	If L98color(CurrentPlayer) = "magenta"	Then L98.color = RGB(255, 0, 255):L98.colorfull = RGB(255, 0, 255):End If
	If L103color(CurrentPlayer)= "magenta"	Then L103.color = RGB(255, 0, 255):L103.colorfull = RGB(255, 0, 255):End If
	End If
End Sub			'CheckLamp

Sub MissionHit(SWNumber)
	If BlackKnightRetro(CurrentPlayer) = 3 Then 	'**************************************** BK2000 MODE RETRO ******************************************
		If SWNumber = 51 Then
			AddScore 2000000
		End If
	End If

	If CatapultModeFlag = True Then 				'**************************************** Catapult Multiball *****************************************
													'***		SWITCH AVAILABLE : SW41 + SW48 + SW51 + SW59 + SW64
		If SWNumber = 41 Or SWNumber = 45 Then L66State(CurrentPlayer) = 2 End If 			'L66    (SW41 + SW45) 	-> Left spinner lane
		If SWNumber = 48 Then L72State(CurrentPlayer) = 2 End If 							'L72					-> Left orbit
		If SWNumber = 51 Then L79State(CurrentPlayer) = 2 End If 							'L79					-> Left ramp
		If SWNumber = 56 Then L85State(CurrentPlayer) = 2 End If 							'L85					-> The Black Knight
		If SWNumber = 58 Or SWNumber = 60 Then L91State(CurrentPlayer) = 2 End If 			'L91	(SW58 + SW60)	-> Shield and rear of shield(saucer)
		If SWNumber = 59 Then L98State(CurrentPlayer) = 2 End If 							'L98					-> Light Lock target 
		If SWNumber = 64 Then L103State(CurrentPlayer) = 2 End If 							'L103					-> Right orbit
		'DOF 56, DOFPulse
		If LastChanceBallInCatapult.Enabled = False Then
			CommentDisplayed = "Catapult Jackpot!"
			LastChanceCatapultMulti = 1
			Select Case Int(Rnd*3)+1
				Case 1 : audioknight = "Sound-0x0293.mp3":SpeakTime = 1119
				Case 2 : audioknight = "Sound-0x0293.mp3":SpeakTime = 1119
				Case 3 : audioknight = "Sound-0x0343.mp3":SpeakTime = 1042
			End Select	
		Else 
			CommentDisplayed = "2x Catapult Jackpot!"
			LastChanceCatapultMulti = 2
			Select Case Int(Rnd*3)+1
				Case 1 : audioknight = "Sound-0x018F.mp3":SpeakTime = 1666
				Case 2 : audioknight = "Sound-0x01E7.mp3":SpeakTime = 1688
				Case 3 : audioknight = "Sound-0x01E7.mp3":SpeakTime = 1688
			End Select	
		End If
		AddscoreCatapultBonus = AddscoreCatapultBonus + 25000
		AddscoreCatapultBonusAfterMulti = AddscoreCatapultBonus * LastChanceCatapultMulti
		Catapult_jackpot_collected = Catapult_jackpot_collected + AddscoreCatapultBonusAfterMulti
		Catapult_Mode_Total_score(CurrentPlayer) = Catapult_Mode_Total_score(CurrentPlayer) + AddscoreCatapultBonusAfterMulti
		AddscoreCatapultTotalEndGame(CurrentPlayer) = AddscoreCatapultTotalEndGame(CurrentPlayer) + AddscoreCatapultBonusAfterMulti
		Addscore AddscoreCatapultBonusAfterMulti
		LightEyesBK
		playmedia audioknight,"Audioknight",pAudio,"",0,"",1,1  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		Video_Catapult_Multball_1
	End If										

	If Knight_challenge_flag = True	Then			'**************************************** Knight Challenge *******************************************
		If Knight_challenge_phase = 1 Then			'***		SWITCH AVAILABLE : SW41 + SW48 + SW51 + SW59 + SW64 
			If SWNumber = 41 or SWNumber = 45 Then	
				L66State(CurrentPlayer) = 0
				KnightRemaining = KnightRemaining - 1
				AddScoreTripleKnightChallenge = AddScoreTripleKnightChallengeKnightHited
				AddScoreTripleKnightChallengeTotal(CurrentPlayer) = AddScoreTripleKnightChallengeTotal(CurrentPlayer) + AddScoreTripleKnightChallenge
				AddScoreTripleKnightChallengeTotalEndGame(CurrentPlayer) = AddScoreTripleKnightChallengeTotalEndGame(CurrentPlayer) + AddScoreTripleKnightChallenge
				Video_knight_challenge_phase1_hited
			End If
			If SWNumber = 51 Then
				L79State(CurrentPlayer) = 0
				KnightRemaining = KnightRemaining - 1
				AddScoreTripleKnightChallenge = AddScoreTripleKnightChallengeKnightHited
				AddScoreTripleKnightChallengeTotal(CurrentPlayer) = AddScoreTripleKnightChallengeTotal(CurrentPlayer) + AddScoreTripleKnightChallenge
				AddScoreTripleKnightChallengeTotalEndGame(CurrentPlayer) = AddScoreTripleKnightChallengeTotalEndGame(CurrentPlayer) + AddScoreTripleKnightChallenge
				Video_knight_challenge_phase1_hited
			End If
			If SWNumber = 58 or SWNumber = 60 Then
				L91State(CurrentPlayer) = 0
				KnightRemaining = KnightRemaining - 1
				AddScoreTripleKnightChallenge = AddScoreTripleKnightChallengeKnightHited
				AddScoreTripleKnightChallengeTotal(CurrentPlayer) = AddScoreTripleKnightChallengeTotal(CurrentPlayer) + AddScoreTripleKnightChallenge
				AddScoreTripleKnightChallengeTotalEndGame(CurrentPlayer) = AddScoreTripleKnightChallengeTotalEndGame(CurrentPlayer) + AddScoreTripleKnightChallenge
				Video_knight_challenge_phase1_hited
			End If
			If KnightRemaining <= 0 Then
				L79color(CurrentPlayer) = "blue"
				L85color(CurrentPlayer) = "blue"
				L91color(CurrentPlayer) = "blue"
				Light055.BlinkPattern="00010000"
				Light059.BlinkPattern="00010000"
				Light054.BlinkPattern="00001000"
				Light060.BlinkPattern="00001000"
				Light056.BlinkPattern="00000100"
				Light061.BlinkPattern="00000100"
				Light057.BlinkPattern="00000010"
				Light058.BlinkPattern="00000010"
				Light062.BlinkPattern="00000001"
				Light063.BlinkPattern="00000001"
				RampLightsBlinkInterval = 125
				RampLightsState = 2
				L83.BlinkPattern="10000000"
				L82.BlinkPattern="01000000"
				L79.BlinkPattern="00100000"
				L83.State = 2
				L82.State = 2
				L79State(CurrentPlayer) = 2
				L85State(CurrentPlayer) = 0
				L91State(CurrentPlayer) = 0
				L66State(CurrentPlayer) = 0
				L72State(CurrentPlayer) = 0
				L98State(CurrentPlayer) = 0
				L103State(CurrentPlayer) = 0
				L83.BlinkInterval=125
				L82.BlinkInterval=125
				L79.BlinkInterval=125
				Knight_challenge_phase = 2
			End If
		ElseIf Knight_challenge_phase = 2 Then
			If SWNumber = 51 Then
				L83.State = 0
				L82.State = 0
				L66State(CurrentPlayer) = 2
				L72State(CurrentPlayer) = 2
				L79State(CurrentPlayer) = 2
				L85State(CurrentPlayer) = 2
				L91State(CurrentPlayer) = 2
				L98State(CurrentPlayer) = 2
				L103State(CurrentPlayer) = 2
				Knight_challenge_phase = 3
				knight_challenge_jackpot = 1
				video_knight_challenge_advance_to_gold_room
				AddScoreTripleKnightChallenge = AddScoreTripleKnightChallengeKnightHited
				AddScoreTripleKnightChallengeTotal(CurrentPlayer) = AddScoreTripleKnightChallengeTotal(CurrentPlayer) + AddScoreTripleKnightChallenge
				AddScoreTripleKnightChallengeTotalEndGame(CurrentPlayer) = AddScoreTripleKnightChallengeTotalEndGame(CurrentPlayer) + AddScoreTripleKnightChallenge
			End If
		ElseIf Knight_challenge_phase = 3 Then
			knight_challenge_jackpot = 2
			
			If SWNumber = 41 Then L66State(CurrentPlayer) = 0 : video_knight_challenge_jackpot : AddScoreTripleKnightChallenge = AddScoreTripleKnightChallengeGoldRoom : AddScoreTripleKnightChallengeTotal(CurrentPlayer) = AddScoreTripleKnightChallengeTotal(CurrentPlayer) + AddScoreTripleKnightChallenge : End If 	'L66    (SW41 + SW45) 	-> Left spinner lane
			If SWNumber = 48 Then L72State(CurrentPlayer) = 0 : video_knight_challenge_jackpot : AddScoreTripleKnightChallenge = AddScoreTripleKnightChallengeGoldRoom : AddScoreTripleKnightChallengeTotal(CurrentPlayer) = AddScoreTripleKnightChallengeTotal(CurrentPlayer) + AddScoreTripleKnightChallenge : End If 	'L72					-> Left orbit
			If SWNumber = 51 Then L79State(CurrentPlayer) = 0 : video_knight_challenge_jackpot : AddScoreTripleKnightChallenge = AddScoreTripleKnightChallengeGoldRoom : AddScoreTripleKnightChallengeTotal(CurrentPlayer) = AddScoreTripleKnightChallengeTotal(CurrentPlayer) + AddScoreTripleKnightChallenge : End If 	'L79					-> Left ramp
			If SWNumber = 56 Then L85State(CurrentPlayer) = 0 : video_knight_challenge_jackpot : AddScoreTripleKnightChallenge = AddScoreTripleKnightChallengeGoldRoom : AddScoreTripleKnightChallengeTotal(CurrentPlayer) = AddScoreTripleKnightChallengeTotal(CurrentPlayer) + AddScoreTripleKnightChallenge : End If 	'L85					-> The Black Knight
			If SWNumber = 58 Then L91State(CurrentPlayer) = 0 : video_knight_challenge_jackpot : AddScoreTripleKnightChallenge = AddScoreTripleKnightChallengeGoldRoom : AddScoreTripleKnightChallengeTotal(CurrentPlayer) = AddScoreTripleKnightChallengeTotal(CurrentPlayer) + AddScoreTripleKnightChallenge : End If 	'L91	(SW58 + SW60)	-> Shield and rear of shield(saucer)
			If SWNumber = 59 Then L98State(CurrentPlayer) = 0 : video_knight_challenge_jackpot : AddScoreTripleKnightChallenge = AddScoreTripleKnightChallengeGoldRoom : AddScoreTripleKnightChallengeTotal(CurrentPlayer) = AddScoreTripleKnightChallengeTotal(CurrentPlayer) + AddScoreTripleKnightChallenge : End If 	'L98					-> Light Lock target 
			If SWNumber = 64 Then L103State(CurrentPlayer) = 0 : video_knight_challenge_jackpot : AddScoreTripleKnightChallenge = AddScoreTripleKnightChallengeGoldRoom : AddScoreTripleKnightChallengeTotal(CurrentPlayer) = AddScoreTripleKnightChallengeTotal(CurrentPlayer) + AddScoreTripleKnightChallenge : End If 	'L103					-> Right orbit
			If L66State(CurrentPlayer) = 0 And L72State(CurrentPlayer) = 0 And L79State(CurrentPlayer) = 0 And L85State(CurrentPlayer) = 0 And L91State(CurrentPlayer) = 0 And L98State(CurrentPlayer) = 0 And L103State(CurrentPlayer) = 0 Then
				Knight_challenge_phase = 1
				KnightRemaining = 3
				L66State(CurrentPlayer) = 2
				L79State(CurrentPlayer) = 2
				L91State(CurrentPlayer) = 2
				video_knight_challenge_background KnightRemaining
			End If
		Else
		End If
	AddScore AddScoreTripleKnightChallenge
	End If





	If WarHurryFlag = True Then					'**************************************** WAR HURRY UP ***********************************************
		If WarHurryPhase = 1 Then
			
			AddScoreWarHurryUpTotal(CurrentPlayer) = AddScoreWarHurryUpTotal(CurrentPlayer) + AddScoreWarHurryUp
			AddScoreWarHurryUpTotalEndGame(CurrentPlayer) = AddScoreWarHurryUpTotalEndGame(CurrentPlayer) + AddScoreWarHurryUp
'			pSplashCommentDisplayed "Burning Sands Total",3,255
'			pSplashCommentDisplayed2 "Burning Sands Total",3,40
			pSplashAddScoreDisplayed "" & FormatNumber(AddScoreWarHurryUp,0),3,255
			pSplashAddScoreDisplayed2 "" & FormatNumber(AddScoreWarHurryUp,0),3,40
			AddScoreWarHurryUp = AddScoreWarHurryUp + 250000
			If SWNumber = 51 Then L79State(CurrentPlayer) = 0:WarHurryUp51Touch=1:End If
			If SWNumber = 56 Then L85State(CurrentPlayer) = 0:WarHurryUp56Touch=1:End If 							'L85					-> The Black Knight
			If SWNumber = 58 Or SWNumber = 60 Then L91State(CurrentPlayer) = 0:WarHurryUp58Touch=1:End If 			'L91	(SW58 + SW60)	-> Shield and rear of shield(saucer)
			If WarHurryUp51Touch = 1 And WarHurryUp56Touch = 1 And WarHurryUp58Touch = 1 Then
				WarHurryPhase = 2
				L85State(CurrentPlayer) = 2
				WarHurryUp51Touch = 0
				WarHurryUp56Touch = 0
				WarHurryUp58Touch = 0
				MusicCheck 200
			End If
		Elseif WarHurryPhase = 2 Then
			AddScoreWarHurryUp = AddScoreWarHurryUp + 250000
			AddScoreWarHurryUpTotal(CurrentPlayer) = AddScoreWarHurryUpTotal(CurrentPlayer) + AddScoreWarHurryUp
			AddScoreWarHurryUpTotalEndGame(CurrentPlayer) = AddScoreWarHurryUpTotalEndGame(CurrentPlayer) + AddScoreWarHurryUp
			pSplashAddScoreDisplayed "" & FormatNumber(AddScoreWarHurryUp,0),3,255
			pSplashAddScoreDisplayed2 "" & FormatNumber(AddScoreWarHurryUp,0),3,40
			If SWNumber = 56 Then L85State(CurrentPlayer) = 0 End If 							'L85					-> The Black Knight
			L85State(CurrentPlayer) = 0
			WarHurryPhase = 0
			WarHurryFlag = False
			KnightLamp(CurrentPlayer) = KnightLamp(CurrentPlayer) + 1
			If KnightLamp(CurrentPlayer) > 12 Then KnightLamp(CurrentPlayer) =12 End if
			Video_Knight
			ModeTotalWarHurry.Interval = 3500
			ModeTotalWarHurry.Enabled = True
			VideoSetBackground
			MusicCheck 22
		Else
			WarHurryFlag = False
			VideoSetBackground
			MusicCheck 22
		End If
	End If

'		If SWNumber = 41 Or SWNumber = 45 Then L66State(CurrentPlayer) = 2 End If 			'L66    (SW41 + SW45) 	-> Left spinner lane
'		If SWNumber = 48 Then L72State(CurrentPlayer) = 2 End If 							'L72					-> Left orbit
'		If SWNumber = 51 Then L79State(CurrentPlayer) = 2 End If 							'L79					-> Left ramp
'		If SWNumber = 56 Then L85State(CurrentPlayer) = 2 End If 							'L85					-> The Black Knight
'		If SWNumber = 58 Or SWNumber = 60 Then L91State(CurrentPlayer) = 2 End If 			'L91	(SW58 + SW60)	-> Shield and rear of shield(saucer)
'		If SWNumber = 59 Then L98State(CurrentPlayer) = 2 End If 							'L98					-> Light Lock target 
'		If SWNumber = 64 Then L103State(CurrentPlayer) = 2 End If 							'L103					-> Right orbit



' AddScoreMudBogSliced
' AddScoreMudBogSealed
' AddScoreMudBogTotal(4)
' AddScoreMudBogBonusLoopSliced
' AddScoreMudBogBonusLoopSealed


	If MudBog(CurrentPlayer) = 2 Then				'**************************************** MUDBOG *****************************************************
		If MudBogDefeated(CurrentPlayer) = 1 Then	'***		SWITCH AVAILABLE : SW41 + SW48 + SW51 + SW59 + SW64
			horloge.Enabled=True
		End If
		If SWNumber = 41 Or SWNumber = 45 Then					'***********  SW41 or SW45 Hited ***********
			If SW41_Flag = 0 Then
				L66State(CurrentPlayer) = 2
				L72State(CurrentPlayer) = 0
				L79State(CurrentPlayer) = 0
				L85State(CurrentPlayer) = 0
				L91State(CurrentPlayer) = 0
				L98State(CurrentPlayer) = 0
				L103State(CurrentPlayer) = 0
				SW41_Flag = 1
				AddScoreMudBogSliced = AddScoreMudBogSliced + 250000 + AddScoreMudBogBonusLoopSliced
				AddScoreMudBogTotal(CurrentPlayer) = AddScoreMudBogTotal(CurrentPlayer) + AddScoreMudBogSliced
				AddScoreMudBogTotalEndGame(CurrentPlayer) = AddScoreMudBogTotalEndGame(CurrentPlayer) + AddScoreMudBogSliced
				AddScoreMudBog = AddScoreMudBogSliced 											'pSplashAddScore
				AddScore AddScoreMudBogSliced
				CommentDisplayed = "Hydra Sliced!"
			Else
				L66State(CurrentPlayer) = 0
				L72State(CurrentPlayer) = 2
				L79State(CurrentPlayer) = 2
				L85State(CurrentPlayer) = 0
				L91State(CurrentPlayer) = 0
				L98State(CurrentPlayer) = 2
				L103State(CurrentPlayer) = 2
				SW41_Flag = 0
				AddScoreMudBogSealed = AddScoreMudBogSealed + 250000 + AddScoreMudBogBonusLoopSealed
				AddScoreMudBogTotal(CurrentPlayer) = AddScoreMudBogTotal(CurrentPlayer) + AddScoreMudBogSealed
				AddScoreMudBogTotalEndGame(CurrentPlayer) = AddScoreMudBogTotalEndGame(CurrentPlayer) + AddScoreMudBogSealed
				AddScoreMudBog = AddScoreMudBogSealed											'pSplashAddScore
				AddScore AddScoreMudBogSealed
				CommentDisplayed = "Hydra Sealed!"
			End If
		End If
		If SWNumber = 48 Then					'***********  SW48 Hited ***********
			If SW48_Flag = 0 Then
				L66State(CurrentPlayer) = 0
				L72State(CurrentPlayer) = 2
				L79State(CurrentPlayer) = 0
				L85State(CurrentPlayer) = 0
				L91State(CurrentPlayer) = 0
				L98State(CurrentPlayer) = 0
				L103State(CurrentPlayer) = 0
				SW48_Flag = 1
				AddScoreMudBogSliced = AddScoreMudBogSliced + 250000 + AddScoreMudBogBonusLoopSliced
				AddScoreMudBogTotal(CurrentPlayer) = AddScoreMudBogTotal(CurrentPlayer) + AddScoreMudBogSliced
				AddScoreMudBogTotalEndGame(CurrentPlayer) = AddScoreMudBogTotalEndGame(CurrentPlayer) + AddScoreMudBogSliced
				AddScoreMudBog = AddScoreMudBogSliced											'pSplashAddScore
				AddScore AddScoreMudBogSliced
				CommentDisplayed = "Hydra Sliced!"
			Else
				L66State(CurrentPlayer) = 2
				L72State(CurrentPlayer) = 0
				L79State(CurrentPlayer) = 2
				L85State(CurrentPlayer) = 0
				L91State(CurrentPlayer) = 0
				L98State(CurrentPlayer) = 2
				L103State(CurrentPlayer) = 2
				SW48_Flag = 0
				AddScoreMudBogSealed = AddScoreMudBogSealed + 250000 + AddScoreMudBogBonusLoopSealed
				AddScoreMudBogTotal(CurrentPlayer) = AddScoreMudBogTotal(CurrentPlayer) + AddScoreMudBogSealed
				AddScoreMudBogTotalEndGame(CurrentPlayer) = AddScoreMudBogTotalEndGame(CurrentPlayer) + AddScoreMudBogSealed
				AddScoreMudBog = AddScoreMudBogSealed											'pSplashAddScore
				AddScore AddScoreMudBogSealed
				CommentDisplayed = "Hydra Sealed!"
			End If
		End If

		If SWNumber = 51 Then					'***********  SW51 Hited ***********
			If SW51_Flag = 0 Then
				L66State(CurrentPlayer) = 0
				L72State(CurrentPlayer) = 0
				L79State(CurrentPlayer) = 2
				L85State(CurrentPlayer) = 0
				L91State(CurrentPlayer) = 0
				L98State(CurrentPlayer) = 0
				L103State(CurrentPlayer) = 0
				SW51_Flag = 1
				AddScoreMudBogSliced = AddScoreMudBogSliced + 250000 + AddScoreMudBogBonusLoopSliced
				AddScoreMudBogTotal(CurrentPlayer) = AddScoreMudBogTotal(CurrentPlayer) + AddScoreMudBogSliced
				AddScoreMudBogTotalEndGame(CurrentPlayer) = AddScoreMudBogTotalEndGame(CurrentPlayer) + AddScoreMudBogSliced
				AddScoreMudBog = AddScoreMudBogSliced											'pSplashAddScore
				AddScore AddScoreMudBogSliced
				CommentDisplayed = "Hydra Sliced!"
			Else
				L66State(CurrentPlayer) = 2
				L72State(CurrentPlayer) = 2
				L79State(CurrentPlayer) = 0
				L85State(CurrentPlayer) = 0
				L91State(CurrentPlayer) = 0
				L98State(CurrentPlayer) = 2
				L103State(CurrentPlayer) = 2
				SW51_Flag = 0
				AddScoreMudBogSealed = AddScoreMudBogSealed + 250000 + AddScoreMudBogBonusLoopSealed
				AddScoreMudBogTotal(CurrentPlayer) = AddScoreMudBogTotal(CurrentPlayer) + AddScoreMudBogSealed
				AddScoreMudBogTotalEndGame(CurrentPlayer) = AddScoreMudBogTotalEndGame(CurrentPlayer) + AddScoreMudBogSealed
				AddScoreMudBog = AddScoreMudBogSealed											'pSplashAddScore
				AddScore AddScoreMudBogSealed
				CommentDisplayed = "Hydra Sealed!"
			End If
		End If

		If SWNumber = 59 Then					'***********  SW59 Hited ***********
			If SW59_Flag = 0 Then
				L66State(CurrentPlayer) = 0
				L72State(CurrentPlayer) = 0
				L79State(CurrentPlayer) = 0
				L85State(CurrentPlayer) = 0
				L91State(CurrentPlayer) = 0
				L98State(CurrentPlayer) = 2
				L103State(CurrentPlayer) = 0
				SW59_Flag = 1
				AddScoreMudBogSliced = AddScoreMudBogSliced + 250000 + AddScoreMudBogBonusLoopSliced
				AddScoreMudBogTotal(CurrentPlayer) = AddScoreMudBogTotal(CurrentPlayer) + AddScoreMudBogSliced
				AddScoreMudBogTotalEndGame(CurrentPlayer) = AddScoreMudBogTotalEndGame(CurrentPlayer) + AddScoreMudBogSliced
				AddScoreMudBog = AddScoreMudBogSliced											'pSplashAddScore
				AddScore AddScoreMudBogSliced
				CommentDisplayed = "Hydra Sliced!"
			Else
				L66State(CurrentPlayer) = 2
				L72State(CurrentPlayer) = 2
				L79State(CurrentPlayer) = 2
				L85State(CurrentPlayer) = 0
				L91State(CurrentPlayer) = 0
				L98State(CurrentPlayer) = 0
				L103State(CurrentPlayer) = 2
				SW59_Flag = 0
				AddScoreMudBogSealed = AddScoreMudBogSealed + 250000 + AddScoreMudBogBonusLoopSealed
				AddScoreMudBogTotal(CurrentPlayer) = AddScoreMudBogTotal(CurrentPlayer) + AddScoreMudBogSealed
				AddScoreMudBogTotalEndGame(CurrentPlayer) = AddScoreMudBogTotalEndGame(CurrentPlayer) + AddScoreMudBogSealed
				AddScoreMudBog = AddScoreMudBogSealed											'pSplashAddScore
				AddScore AddScoreMudBogSealed
				CommentDisplayed = "Hydra Sealed!"
			End If
		End If

		If SWNumber = 64 Then					'***********  SW64 Hited ***********
			If SW64_Flag = 0 Then
				L66State(CurrentPlayer) = 0
				L72State(CurrentPlayer) = 0
				L79State(CurrentPlayer) = 0
				L85State(CurrentPlayer) = 0
				L91State(CurrentPlayer) = 0
				L98State(CurrentPlayer) = 0
				L103State(CurrentPlayer) = 2
				SW64_Flag = 1
				AddScoreMudBogSliced = AddScoreMudBogSliced + 250000 + AddScoreMudBogBonusLoopSliced
				AddScoreMudBogTotal(CurrentPlayer) = AddScoreMudBogTotal(CurrentPlayer) + AddScoreMudBogSliced
				AddScoreMudBogTotalEndGame(CurrentPlayer) = AddScoreMudBogTotalEndGame(CurrentPlayer) + AddScoreMudBogSliced
				AddScoreMudBog = AddScoreMudBogSliced											'pSplashAddScore
				AddScore AddScoreMudBogSliced
				CommentDisplayed = "Hydra Sliced!"
			Else
				L66State(CurrentPlayer) = 2
				L72State(CurrentPlayer) = 2
				L79State(CurrentPlayer) = 2
				L85State(CurrentPlayer) = 0
				L91State(CurrentPlayer) = 0
				L98State(CurrentPlayer) = 2
				L103State(CurrentPlayer) = 0
				SW64_Flag = 0
				AddScoreMudBogSealed = AddScoreMudBogSealed + 250000 + AddScoreMudBogBonusLoopSealed
				AddScoreMudBogTotal(CurrentPlayer) = AddScoreMudBogTotal(CurrentPlayer) + AddScoreMudBogSealed
				AddScoreMudBogTotalEndGame(CurrentPlayer) = AddScoreMudBogTotalEndGame(CurrentPlayer) + AddScoreMudBogSealed
				AddScoreMudBog = AddScoreMudBogSealed											'pSplashAddScore
				AddScore AddScoreMudBogSealed
				CommentDisplayed = "Hydra Sealed!"
			End If
		End If
		If SWNumber = 82 Then
				AddScoreMudBogSliced = AddScoreMudBogSliced + 250000 + AddScoreMudBogBonusLoopSliced
				AddScoreMudBogSealed = AddScoreMudBogSealed + 250000 + AddScoreMudBogBonusLoopSealed
				AddScoreMudBogTotal(CurrentPlayer) = AddScoreMudBogTotal(CurrentPlayer) + AddScoreMudBogSealed + AddScoreMudBogSliced
				AddScoreMudBogTotalEndGame(CurrentPlayer) = AddScoreMudBogTotalEndGame(CurrentPlayer) + AddScoreMudBogSealed + AddScoreMudBogSliced
				AddScoreMudBog = AddScoreMudBogSealed											'pSplashAddScore
				AddScore AddScoreMudBogSliced
				AddScore AddScoreMudBogSealed
				CommentDisplayed = "Hydra Sealed!"
		End If
		playclear pBackglass
		VideoHydraattack.Enabled = True					
		SW60Timer=3000
	End If

	If MoltenFire(CurrentPlayer) = 2 Then		'**************************************** MoltenFire *****************************************************
												'***		SWITCH AVAILABLE : SW41 + SW48 + SW51 + SW56 + SW58 + SW59 + SW64
		SW60Timer=3000

		If MoltenFireDefeated(CurrentPlayer) < 7 Then
			If SWNumber = 41 or SWNumber = 45 Then 												'L66    (SW41 + SW45) 	-> Left spinner lane
				L66State(CurrentPlayer) = 0
				AddScoreMoltenFire = AddScoreMoltenFire + AddScoreMoltenFireBonusLoop
				AddScoreMoltenFireTotal(CurrentPlayer) = AddScoreMoltenFireTotal(CurrentPlayer) + AddScoreMoltenFire
				AddScoreMoltenFireTotalEndGame(CurrentPlayer) = AddScoreMoltenFireTotalEndGame(CurrentPlayer) + AddScoreMoltenFire				
				CommentDisplayed = "Magma Beast Award!"
			End If 	
			If SWNumber = 48 Then 												'L72					-> Left orbit
				L72State(CurrentPlayer) = 0 
				AddScoreMoltenFire = AddScoreMoltenFire + AddScoreMoltenFireBonusLoop
				AddScoreMoltenFireTotal(CurrentPlayer) = AddScoreMoltenFireTotal(CurrentPlayer) + AddScoreMoltenFire
				AddScoreMoltenFireTotalEndGame(CurrentPlayer) = AddScoreMoltenFireTotalEndGame(CurrentPlayer) + AddScoreMoltenFire		
				CommentDisplayed = "Magma Beast Award!"
			End If
			If SWNumber = 51 Then 												'L79					-> Left ramp
				L79State(CurrentPlayer) = 0 
				AddScoreMoltenFire = AddScoreMoltenFire + AddScoreMoltenFireBonusLoop
				AddScoreMoltenFireTotal(CurrentPlayer) = AddScoreMoltenFireTotal(CurrentPlayer) + AddScoreMoltenFire
				AddScoreMoltenFireTotalEndGame(CurrentPlayer) = AddScoreMoltenFireTotalEndGame(CurrentPlayer) + AddScoreMoltenFire		
				CommentDisplayed = "Magma Beast Award!"
				End If 	
			If SWNumber = 56 Then 												'L85					-> The Black Knight
				L85State(CurrentPlayer) = 0 
				AddScoreMoltenFire = AddScoreMoltenFire + AddScoreMoltenFireBonusLoop
				AddScoreMoltenFireTotal(CurrentPlayer) = AddScoreMoltenFireTotal(CurrentPlayer) + AddScoreMoltenFire
				AddScoreMoltenFireTotalEndGame(CurrentPlayer) = AddScoreMoltenFireTotalEndGame(CurrentPlayer) + AddScoreMoltenFire		
				CommentDisplayed = "Magma Beast Award!"
			End If 	
			If SWNumber = 58 Then 												'L91	(SW58 + SW60)	-> Shield and rear of shield(saucer)
				L91State(CurrentPlayer) = 0 
				AddScoreMoltenFire = AddScoreMoltenFire + AddScoreMoltenFireBonusLoop
				AddScoreMoltenFireTotal(CurrentPlayer) = AddScoreMoltenFireTotal(CurrentPlayer) + AddScoreMoltenFire
				AddScoreMoltenFireTotalEndGame(CurrentPlayer) = AddScoreMoltenFireTotalEndGame(CurrentPlayer) + AddScoreMoltenFire		
				CommentDisplayed = "Magma Beast Award!"
			End If 	
			If SWNumber = 59 Then 												'L98					-> Light Lock target 
				L98State(CurrentPlayer) = 0 
				AddScoreMoltenFire = AddScoreMoltenFire + AddScoreMoltenFireBonusLoop
				AddScoreMoltenFireTotal(CurrentPlayer) = AddScoreMoltenFireTotal(CurrentPlayer) + AddScoreMoltenFire
				AddScoreMoltenFireTotalEndGame(CurrentPlayer) = AddScoreMoltenFireTotalEndGame(CurrentPlayer) + AddScoreMoltenFire		
				CommentDisplayed = "Magma Beast Award!"
				End If 	
			If SWNumber = 64 Then 												'L103					-> Right orbit
				L103State(CurrentPlayer) = 0 
				AddScoreMoltenFire = AddScoreMoltenFire + AddScoreMoltenFireBonusLoop
				AddScoreMoltenFireTotal(CurrentPlayer) = AddScoreMoltenFireTotal(CurrentPlayer) + AddScoreMoltenFire
				AddScoreMoltenFireTotalEndGame(CurrentPlayer) = AddScoreMoltenFireTotalEndGame(CurrentPlayer) + AddScoreMoltenFire		
				CommentDisplayed = "Magma Beast Award!"
			End If 	
			If SWNumber = 82 Then 												'L103					-> Right orbit
				AddScoreMoltenFire = AddScoreMoltenFire + AddScoreMoltenFireBonusLoop
				AddScoreMoltenFireTotal(CurrentPlayer) = AddScoreMoltenFireTotal(CurrentPlayer) + AddScoreMoltenFire
				AddScoreMoltenFireTotalEndGame(CurrentPlayer) = AddScoreMoltenFireTotalEndGame(CurrentPlayer) + AddScoreMoltenFire		
				CommentDisplayed = "Magma Beast Award!"
			End If 
		End If
		Addscore AddScoreMoltenFire
		playclear pBackglass
		VideoFireElementattack.Enabled = True	
	End If

	If BurningSands(CurrentPlayer) = 2 Then		'**************************************** Burning Sands **************************************************	
		SW60Timer=3000
		BurningSandsCount = 0
		If BurningSandsHit = 0 Then
			If SWNumber = 41 or SWNumber = 45 Then L66State(CurrentPlayer) = 0:L72State(CurrentPlayer) = 2:L79State(CurrentPlayer) = 2:End If 	'L66    (SW41 + SW45) 	-> Left spinner lane
			If SWNumber = 48 Then L66State(CurrentPlayer) = 2:L72State(CurrentPlayer) = 0:L79State(CurrentPlayer) = 2:End If 	'L72					-> Left orbit
			If SWNumber = 51 Then L72State(CurrentPlayer) = 2:L79State(CurrentPlayer) = 0:L85State(CurrentPlayer) = 2:End If 	'L79					-> Left ramp
			If SWNumber = 56 Then L79State(CurrentPlayer) = 2:L85State(CurrentPlayer) = 0:L91State(CurrentPlayer) = 2:End If 	'L85					-> The Black Knight
			If SWNumber = 58 Then L85State(CurrentPlayer) = 2:L91State(CurrentPlayer) = 0:L98State(CurrentPlayer) = 2:End If 	'L91	(SW58 + SW60)	-> Shield and rear of shield(saucer)
			If SWNumber = 59 Then L91State(CurrentPlayer) = 2:L98State(CurrentPlayer) = 0:L103State(CurrentPlayer) =2:End If 	'L98					-> Light Lock target 
			If SWNumber = 64 Then L91State(CurrentPlayer) = 2:L98State(CurrentPlayer) = 2:L103State(CurrentPlayer) =0:End If 	'L103					-> Right orbit
			BurningSandsHit = 1
		ElseIf BurningSandsHit = 1 Then
			If SWNumber = 41 or SWNumber = 45 Then L66State(CurrentPlayer) = 0 End If 	'L66    (SW41 + SW45) 	-> Left spinner lane
			If SWNumber = 48 Then L72State(CurrentPlayer) = 0 End If 	'L72					-> Left orbit
			If SWNumber = 51 Then L79State(CurrentPlayer) = 0 End If 	'L79					-> Left ramp
			If SWNumber = 56 Then L85State(CurrentPlayer) = 0 End If 	'L85					-> The Black Knight
			If SWNumber = 58 Then L91State(CurrentPlayer) = 0 End If 	'L91	(SW58 + SW60)	-> Shield and rear of shield(saucer)
			If SWNumber = 59 Then L98State(CurrentPlayer) = 0 End If 	'L98					-> Light Lock target 
			If SWNumber = 64 Then L103State(CurrentPlayer) = 0 End If 	'L103					-> Right orbit
			BurningSandsHit = 2
		ElseIf BurningSandsHit = 2 Then
			If SWNumber = 41 or SWNumber = 45 Then L66State(CurrentPlayer) = 0 End If 	'L66    (SW41 + SW45) 	-> Left spinner lane
			If SWNumber = 48 Then L72State(CurrentPlayer) = 0 End If 	'L72					-> Left orbit
			If SWNumber = 51 Then L79State(CurrentPlayer) = 0 End If 	'L79					-> Left ramp
			If SWNumber = 56 Then L85State(CurrentPlayer) = 0 End If 	'L85					-> The Black Knight
			If SWNumber = 58 Then L91State(CurrentPlayer) = 0 End If 	'L91	(SW58 + SW60)	-> Shield and rear of shield(saucer)
			If SWNumber = 59 Then L98State(CurrentPlayer) = 0 End If 	'L98					-> Light Lock target 
			If SWNumber = 64 Then L103State(CurrentPlayer) = 0 End If 	'L103					-> Right orbit
			BurningSandsHit = 0
		Else
		End If
		AddScoreSandWorm = AddScoreSandWorm + AddScoreSandWormLoop
		AddScoreSandWormTotal(CurrentPlayer) = AddScoreSandWormTotal(CurrentPlayer) + AddScoreSandWorm
		AddScoreSandWormTotalEndGame(CurrentPlayer) = AddScoreSandWormTotalEndGame(CurrentPlayer) + AddScoreSandWorm
		CommentDisplayed = "Sandworm Award!"
		Addscore AddScoreSandWorm
		playclear pBackglass
		VideoSandwormattack.Enabled = True
	End If

	If WickedCavern(CurrentPlayer) = 2 Then		'**************************************** WickedCavern *****************************************************
		If SWNumber = 0 Then					'***		SWITCH AVAILABLE : SW41 + SW48 + SW51 + SW56 + SW58 + SW59 + SW64
			Select Case Int(Rnd*2)+1
				Case 1 : WickedCavernLeftOrRight = "left": 'Left : SW41+SW48+SW51+SW56
				Case 2 : WickedCavernLeftOrRight = "right":'Right: SW56+SW58+SW59+SW64
			End Select
		Else
			AddScoreWickedCavern = AddScoreWickedCavern + AddScoreWickedCavernLoop
			AddScoreWickedCavernTotal(CurrentPlayer) = AddScoreWickedCavernTotal(CurrentPlayer) + AddScoreWickedCavern
			AddScoreWickedCavernTotalEndGame(CurrentPlayer) = AddScoreWickedCavernTotalEndGame(CurrentPlayer) + AddScoreWickedCavern
			CommentDisplayed = "Hell Hand Award!"
			Addscore AddScoreWickedCavern
			playclear pBackglass
			VideoHandHolderattack.Enabled = True				
			SW60Timer=3000
		End If

		If WickedCavernDefeated(CurrentPlayer) = 0 Then
			WickedCavernCheckCountRandomLeft(CurrentPlayer) = 0
			WickedCavernCheckCountRandomRight(CurrentPlayer) = 0
		End If

		If WickedCavernLeftOrRight = "left" Then
			WickedCavernLeftOrRightOld = WickedCavernLeftOrRight
			WickedCavernLeftOrRight = "right"
			If WickedCavernCheckCountRandomLeft(CurrentPlayer) <= 2 Then
				WickedCavernRandomLeftLight		
			Else
				WickedCavernCheckCountRandomLeft(CurrentPlayer) = WickedCavernCheckCountRandomLeft(CurrentPlayer) +1
			End If

		Else
			WickedCavernLeftOrRightOld = WickedCavernLeftOrRight
			WickedCavernLeftOrRight = "left" 
			If WickedCavernCheckCountRandomRight(CurrentPlayer) <= 2 Then
				WickedCavernRandomRightLight
			Else
				WickedCavernCheckCountRandomRight(CurrentPlayer) = WickedCavernCheckCountRandomRight(CurrentPlayer) +1
			End If
		End If	
	End If

	If DeepFreeze(CurrentPlayer) = 2 Then	'**************************************** DeepFreeze *****************************************************
		DeepFreezeMissionHitStart(CurrentPlayer) = True			'***		SWITCH AVAILABLE : SW41 + SW48 + SW51 + SW56 + SW59 + SW64
		If SWNumber = 41 And L66color(CurrentPlayer) = "blue" Then
			L66color(CurrentPlayer) = "red"
			DeepFreezeTargetRedHit = False
			AddScoreDeepFreeze = AddScoreDeepFreezeFrozen + AddScoreDeepFreezeFrozenBonusLoop
			AddScoreDeepFreezeFrozen = AddScoreDeepFreezeFrozen + 250000
			AddScoreDeepFreezeTotal(CurrentPlayer) = AddScoreDeepFreezeTotal(CurrentPlayer) + AddScoreDeepFreeze
			AddScoreDeepFreezeTotalEndGame(CurrentPlayer) = AddScoreDeepFreezeTotalEndGame(CurrentPlayer) + AddScoreDeepFreeze
			CommentDisplayed = "Lich Lord Frozen!"

		Elseif SWNumber = 41 And L66color(CurrentPlayer) = "red" Then
			DeepFreezeTargetRedHit = True
			AddScoreDeepFreeze = AddScoreDeepFreezeAwarded + AddScoreDeepFreezeAwardedBonusLoop
			AddScoreDeepFreezeAwarded = AddScoreDeepFreezeAwarded + 250000
			AddScoreDeepFreezeTotal(CurrentPlayer) = AddScoreDeepFreezeTotal(CurrentPlayer) + AddScoreDeepFreeze
			AddScoreDeepFreezeTotalEndGame(CurrentPlayer) = AddScoreDeepFreezeTotalEndGame(CurrentPlayer) + AddScoreDeepFreeze
			CommentDisplayed = "Lich Lord Awarded!"
		Else
		End If
		
		If SWNumber = 45 And L66color(CurrentPlayer) = "blue" Then			'Same of SW41
			L66color(CurrentPlayer) = "red"
			DeepFreezeTargetRedHit = False
			AddScoreDeepFreeze = AddScoreDeepFreezeFrozen + AddScoreDeepFreezeFrozenBonusLoop
			AddScoreDeepFreezeFrozen = AddScoreDeepFreezeFrozen + 250000
			AddScoreDeepFreezeTotal(CurrentPlayer) = AddScoreDeepFreezeTotal(CurrentPlayer) + AddScoreDeepFreeze
			AddScoreDeepFreezeTotalEndGame(CurrentPlayer) = AddScoreDeepFreezeTotalEndGame(CurrentPlayer) + AddScoreDeepFreeze
			CommentDisplayed = "Lich Lord Frozen!"
		Elseif SWNumber = 45 And L66color(CurrentPlayer) = "red" Then
			DeepFreezeTargetRedHit = True
			DeepFreezeTargetRedHit = True
			AddScoreDeepFreeze = AddScoreDeepFreezeAwarded + AddScoreDeepFreezeAwardedBonusLoop
			AddScoreDeepFreezeAwarded = AddScoreDeepFreezeAwarded + 250000
			AddScoreDeepFreezeTotal(CurrentPlayer) = AddScoreDeepFreezeTotal(CurrentPlayer) + AddScoreDeepFreeze
			AddScoreDeepFreezeTotalEndGame(CurrentPlayer) = AddScoreDeepFreezeTotalEndGame(CurrentPlayer) + AddScoreDeepFreeze
			CommentDisplayed = "Lich Lord Awarded!"
		Else
		End If

		If SWNumber = 48 And L72color(CurrentPlayer) = "blue" Then
			L72color(CurrentPlayer) = "red"
			DeepFreezeTargetRedHit = False
			AddScoreDeepFreeze = AddScoreDeepFreezeFrozen + AddScoreDeepFreezeFrozenBonusLoop
			AddScoreDeepFreezeFrozen = AddScoreDeepFreezeFrozen + 250000
			AddScoreDeepFreezeTotal(CurrentPlayer) = AddScoreDeepFreezeTotal(CurrentPlayer) + AddScoreDeepFreeze
			AddScoreDeepFreezeTotalEndGame(CurrentPlayer) = AddScoreDeepFreezeTotalEndGame(CurrentPlayer) + AddScoreDeepFreeze
			CommentDisplayed = "Lich Lord Frozen!"
		Elseif SWNumber = 48 And L72color(CurrentPlayer) = "red" Then
			DeepFreezeTargetRedHit = True
			DeepFreezeTargetRedHit = True
			AddScoreDeepFreeze = AddScoreDeepFreezeAwarded + AddScoreDeepFreezeAwardedBonusLoop
			AddScoreDeepFreezeAwarded = AddScoreDeepFreezeAwarded + 250000
			AddScoreDeepFreezeTotal(CurrentPlayer) = AddScoreDeepFreezeTotal(CurrentPlayer) + AddScoreDeepFreeze
			AddScoreDeepFreezeTotalEndGame(CurrentPlayer) = AddScoreDeepFreezeTotalEndGame(CurrentPlayer) + AddScoreDeepFreeze
			CommentDisplayed = "Lich Lord Awarded!"
		Else
		End If

		If SWNumber = 51 And L79color(CurrentPlayer) = "blue" Then
			L79color(CurrentPlayer) = "red"
			DeepFreezeTargetRedHit = False
			AddScoreDeepFreeze = AddScoreDeepFreezeFrozen + AddScoreDeepFreezeFrozenBonusLoop
			AddScoreDeepFreezeFrozen = AddScoreDeepFreezeFrozen + 250000
			AddScoreDeepFreezeTotal(CurrentPlayer) = AddScoreDeepFreezeTotal(CurrentPlayer) + AddScoreDeepFreeze
			AddScoreDeepFreezeTotalEndGame(CurrentPlayer) = AddScoreDeepFreezeTotalEndGame(CurrentPlayer) + AddScoreDeepFreeze
			CommentDisplayed = "Lich Lord Frozen!"
		Elseif SWNumber = 51 And L79color(CurrentPlayer) = "red" Then
			DeepFreezeTargetRedHit = True
			DeepFreezeTargetRedHit = True
			AddScoreDeepFreeze = AddScoreDeepFreezeAwarded + AddScoreDeepFreezeAwardedBonusLoop
			AddScoreDeepFreezeAwarded = AddScoreDeepFreezeAwarded + 250000
			AddScoreDeepFreezeTotal(CurrentPlayer) = AddScoreDeepFreezeTotal(CurrentPlayer) + AddScoreDeepFreeze
			AddScoreDeepFreezeTotalEndGame(CurrentPlayer) = AddScoreDeepFreezeTotalEndGame(CurrentPlayer) + AddScoreDeepFreeze
			CommentDisplayed = "Lich Lord Awarded!"
		Else
		End If

		If SWNumber = 56 And L85color(CurrentPlayer) = "blue" Then
			L85color(CurrentPlayer) = "red"
			DeepFreezeTargetRedHit = False
			AddScoreDeepFreeze = AddScoreDeepFreezeFrozen + AddScoreDeepFreezeFrozenBonusLoop
			AddScoreDeepFreezeFrozen = AddScoreDeepFreezeFrozen + 250000
			AddScoreDeepFreezeTotal(CurrentPlayer) = AddScoreDeepFreezeTotal(CurrentPlayer) + AddScoreDeepFreeze
			AddScoreDeepFreezeTotalEndGame(CurrentPlayer) = AddScoreDeepFreezeTotalEndGame(CurrentPlayer) + AddScoreDeepFreeze
			CommentDisplayed = "Lich Lord Frozen!"
		Elseif SWNumber = 56 And L85color(CurrentPlayer) = "red" Then
			DeepFreezeTargetRedHit = True
			DeepFreezeTargetRedHit = True
			AddScoreDeepFreeze = AddScoreDeepFreezeAwarded + AddScoreDeepFreezeAwardedBonusLoop
			AddScoreDeepFreezeAwarded = AddScoreDeepFreezeAwarded + 250000
			AddScoreDeepFreezeTotal(CurrentPlayer) = AddScoreDeepFreezeTotal(CurrentPlayer) + AddScoreDeepFreeze
			AddScoreDeepFreezeTotalEndGame(CurrentPlayer) = AddScoreDeepFreezeTotalEndGame(CurrentPlayer) + AddScoreDeepFreeze
			CommentDisplayed = "Lich Lord Awarded!"
		Else
		End If

		If SWNumber = 59 And L98color(CurrentPlayer) = "blue" Then
			L98color(CurrentPlayer) = "red"
			DeepFreezeTargetRedHit = False
			AddScoreDeepFreeze = AddScoreDeepFreezeFrozen + AddScoreDeepFreezeFrozenBonusLoop
			AddScoreDeepFreezeFrozen = AddScoreDeepFreezeFrozen + 250000
			AddScoreDeepFreezeTotal(CurrentPlayer) = AddScoreDeepFreezeTotal(CurrentPlayer) + AddScoreDeepFreeze
			AddScoreDeepFreezeTotalEndGame(CurrentPlayer) = AddScoreDeepFreezeTotalEndGame(CurrentPlayer) + AddScoreDeepFreeze
			CommentDisplayed = "Lich Lord Frozen!"
		Elseif SWNumber = 59 And L98color(CurrentPlayer) = "red" Then
			DeepFreezeTargetRedHit = True
			DeepFreezeTargetRedHit = True
			AddScoreDeepFreeze = AddScoreDeepFreezeAwarded + AddScoreDeepFreezeAwardedBonusLoop
			AddScoreDeepFreezeAwarded = AddScoreDeepFreezeAwarded + 250000
			AddScoreDeepFreezeTotal(CurrentPlayer) = AddScoreDeepFreezeTotal(CurrentPlayer) + AddScoreDeepFreeze
			AddScoreDeepFreezeTotalEndGame(CurrentPlayer) = AddScoreDeepFreezeTotalEndGame(CurrentPlayer) + AddScoreDeepFreeze
			CommentDisplayed = "Lich Lord Awarded!"
		Else
		End If

		If SWNumber = 64 And L103color(CurrentPlayer) = "blue" Then
			L103color(CurrentPlayer) = "red"
			DeepFreezeTargetRedHit = False
			AddScoreDeepFreeze = AddScoreDeepFreezeFrozen + AddScoreDeepFreezeFrozenBonusLoop
			AddScoreDeepFreezeFrozen = AddScoreDeepFreezeFrozen + 250000
			AddScoreDeepFreezeTotal(CurrentPlayer) = AddScoreDeepFreezeTotal(CurrentPlayer) + AddScoreDeepFreeze
			AddScoreDeepFreezeTotalEndGame(CurrentPlayer) = AddScoreDeepFreezeTotalEndGame(CurrentPlayer) + AddScoreDeepFreeze
			CommentDisplayed = "Lich Lord Frozen!"
		Elseif SWNumber = 64 And L103color(CurrentPlayer) = "red" Then
			DeepFreezeTargetRedHit = True
			AddScoreDeepFreeze = AddScoreDeepFreezeAwarded + AddScoreDeepFreezeAwardedBonusLoop
			AddScoreDeepFreezeAwarded = AddScoreDeepFreezeAwarded + 250000
			AddScoreDeepFreezeTotal(CurrentPlayer) = AddScoreDeepFreezeTotal(CurrentPlayer) + AddScoreDeepFreeze
			AddScoreDeepFreezeTotalEndGame(CurrentPlayer) = AddScoreDeepFreezeTotalEndGame(CurrentPlayer) + AddScoreDeepFreeze
			CommentDisplayed = "Lich Lord Awarded!"
		Else
		End If

		If SWNumber = 82 Then
			AddScoreDeepFreeze = AddScoreDeepFreezeAwarded + AddScoreDeepFreezeFrozen + AddScoreDeepFreezeFrozenBonusLoop
			AddScoreDeepFreezeFrozen = AddScoreDeepFreezeFrozen + 250000
			AddScoreDeepFreezeAwarded = AddScoreDeepFreezeAwarded + 250000
			AddScoreDeepFreezeTotal(CurrentPlayer) = AddScoreDeepFreezeTotal(CurrentPlayer) + AddScoreDeepFreeze
			AddScoreDeepFreezeTotalEndGame(CurrentPlayer) = AddScoreDeepFreezeTotalEndGame(CurrentPlayer) + AddScoreDeepFreeze
			DeepFreezeTargetRedHit = True
			AddScoreDeepFreeze = AddScoreDeepFreezeAwarded + AddScoreDeepFreezeAwardedBonusLoop
			CommentDisplayed = "Lich Lord Awarded!"
		End If

		If DeepFreezeTargetRedHit = True Then
			If L66State(CurrentPlayer) = 2 And L66color(CurrentPlayer) = "red" Then L66State(CurrentPlayer) = 0:L66color(CurrentPlayer) = "blue":L72State(CurrentPlayer)=2:L72color(CurrentPlayer) = "blue":DeepFreezeDefeated(CurrentPlayer) = DeepFreezeDefeated(CurrentPlayer) + 1:End If
			If L72State(CurrentPlayer) = 2 And L72color(CurrentPlayer) = "red" Then L72State(CurrentPlayer) = 0:L72color(CurrentPlayer) = "blue":L66State(CurrentPlayer)=2:L66color(CurrentPlayer) = "blue":DeepFreezeDefeated(CurrentPlayer) = DeepFreezeDefeated(CurrentPlayer) + 1:End If
			If L79State(CurrentPlayer) = 2 And L79color(CurrentPlayer) = "red" Then L79State(CurrentPlayer) = 0:L79color(CurrentPlayer) = "blue":L85State(CurrentPlayer)=2:L85color(CurrentPlayer) = "blue":DeepFreezeDefeated(CurrentPlayer) = DeepFreezeDefeated(CurrentPlayer) + 1:End If
			If L85State(CurrentPlayer) = 2 And L85color(CurrentPlayer) = "red" Then L85State(CurrentPlayer) = 0:L85color(CurrentPlayer) = "blue":L79State(CurrentPlayer)=2:L79color(CurrentPlayer) = "blue":DeepFreezeDefeated(CurrentPlayer) = DeepFreezeDefeated(CurrentPlayer) + 1:End If
			If L98State(CurrentPlayer) = 2 And L98color(CurrentPlayer) = "red" Then L98State(CurrentPlayer) = 0:L98color(CurrentPlayer) = "blue":L103State(CurrentPlayer)=2:L103color(CurrentPlayer) = "blue":DeepFreezeDefeated(CurrentPlayer) = DeepFreezeDefeated(CurrentPlayer) + 1:End If
			If L103State(CurrentPlayer)= 2 And L103color(CurrentPlayer) = "red" Then L103State(CurrentPlayer)= 0:L103color(CurrentPlayer) = "blue":L98State(CurrentPlayer)=2:L98color(CurrentPlayer) = "blue":DeepFreezeDefeated(CurrentPlayer) = DeepFreezeDefeated(CurrentPlayer) + 1:End If
			DeepFreezeTargetRedHit = False
		End If
		playclear pBackglass
		VideoLichattack.Enabled = True		
		SW60Timer=3000
		AddScore AddScoreDeepFreeze
	End If

	If BlackCastle(CurrentPlayer) = 2 Then '**************************************** BlackCastle *****************************************************
		playclear pBackglass			   '***		SWITCH AVAILABLE : SW41 + SW48 + SW51 + SW56 + SW58 + SW59 + SW64
		VideoBlackCastleattack.Enabled = True  
		SW60Timer=3000
		BlackCastleMissionHitStart(CurrentPlayer) = True
		If L79State(CurrentPlayer) = 0 And L85State(CurrentPlayer) = 0 And L85State(CurrentPlayer) = 0 Then
			L66State(CurrentPlayer) = 2
			L72State(CurrentPlayer) = 2
			L98State(CurrentPlayer) = 2
			L103State(CurrentPlayer) = 2
			L79State(CurrentPlayer) = 0
			L85State(CurrentPlayer) = 0
			L91State(CurrentPlayer) = 0
		End If
		If SWNumber = 51 Then
			L79State(CurrentPlayer) = 0
		End If

		If SWNumber = 56 Then
			L85State(CurrentPlayer) = 0
		End If
		If SWNumber = 58 Then
			L91State(CurrentPlayer) = 0
		End If

		If SWNumber = 41 Or SWNumber = 45 Or SWNumber = 48 Or SWNumber = 59 or SWNumber = 64 Then
			L66State(CurrentPlayer) = 0
			L72State(CurrentPlayer) = 0
			L79State(CurrentPlayer) = 2
			L85State(CurrentPlayer) = 2
			L91State(CurrentPlayer) = 2
			L98State(CurrentPlayer) = 0
			L103State(CurrentPlayer) = 0
			BlackCastleDefeated(CurrentPlayer) = BlackCastleDefeated(CurrentPlayer) + 1
			If BlackCastleDefeated(CurrentPlayer) = 4 Then
				L66State(CurrentPlayer) = 0
				L72State(CurrentPlayer) = 0
				L79State(CurrentPlayer) = 0
				L85State(CurrentPlayer) = 0
				L91State(CurrentPlayer) = 2
				L98State(CurrentPlayer) = 0
				L103State(CurrentPlayer) = 0
			End If
		End If
		AddScore AddScoreBlackCastle
		AddScoreBlackCastleTotal(CurrentPlayer) = AddScoreBlackCastleTotal(CurrentPlayer) + AddScoreBlackCastle
		AddScoreBlackCastleTotalEndGame(CurrentPlayer) = AddScoreBlackCastleTotalEndGame(CurrentPlayer) + AddScoreBlackCastle
	End If
CheckLamp.Enabled = True
End Sub

Sub CheckLampAction_Timer()
'******************** right gate L72 is light when the is enabled for Two Way *******************************

'	If RightGate.Collidable = True Then
'		L75.State=0
'	Else
'		L75.color = RGB(255, 0, 0)	
'		L75.State=1
'	End If



'?????????????????????????????????????????????

'*************************************************************************************************************

	If CountSW45used(CurrentPlayer) = 1 Then
		L70.State=1
	End If
	If CountSW45used(CurrentPlayer) = 0 or CountSW45used(CurrentPlayer) = 2 Then
		L70.State=0
	End If

'*************************************************************************************************************
	If MagnaSaveFlag(CurrentPlayer) = 1 Then
		L24.State = 1
		DOF 201, DOFOn
	End If

	If L110.State = 1 And L111.State = 1 And L112.State = 1 Then
		MagnaSaveFlag(CurrentPlayer) = 1
		DOF 201, DOFOn
		L24.State = 1
		L110.State = 2
		L111.State = 2
		L112.State = 2
	End If


'*************************************************************************************************************

	If L63.State = 1 And L64.State = 1 Then
		BallSaveAvailable(CurrentPlayer) = True		
	End If

	If BallSaveAvailable(CurrentPlayer) = True Then
		'BallSaveAvailable = False
		L13.state = 1 	
		L63.state = 0 
		L64.state = 0 
	Else
		L13.state = 0 	
	End If


If BallsOnPlayfield > 0 Then			' Commented but I don't know the reason...
'IF bGameInPlay Then
	IF L11.state=1 Then
		If L12.state=1 Then 
			If L15.state=1 Then
				If L16.state=1 Then
					'PlaySound "Sound-0x0211"
					If RetroMode = 1 or RetroMode = 3 Then
						'playmedia "Sound-0x0047.mp3","Audionoise",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
					Else
						SpeakTime = 1996
						LightEyesBK
						playmedia "Sound-0x0211.mp3","Audioknight",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority) 'Play sound RAGE!!!!
					    DOF 232, DOFPulse
						RageCount = RageCount + 1
						If RageCount => DifficultyRageCountMystery Then						'!!!!!!!!!! modify after RageCount
							MysteryFlag = True
							L96.State = 2
						End If
					End If
					L11.state=0
					L12.state=0
					L15.state=0
					L16.state=0
				End If
			End If
		End If
	End If



	IF L115.state=1 Then
		IF L116.state=1 Then
			IF L117.state=1 Then
				'PlaySound "Sound-0x01FF"
				If RetroMode = 1 or RetroMode = 3 Then
					'playmedia "Sound-0x0047.mp3","Audionoise",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
				Else
					SpeakTime = 1467
					LightEyesBK
					playmedia "Sound-0x01FF.mp3","Audioknight",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority) 'Play Sound WAR
					WarCount = WarCount + 1 
					If CurrentMissionFlag(CurrentPlayer) = False And WarHurryFlag = False Then
						WarHurryFlag = True
						CheckLamp.Enabled = True
						WarHurryPhase = 1
						MusicCheck 66
						Video_WarHurryUp
						WarHurryTimer.Enabled = True
					End If
				End If
				L115.state=0
				L116.state=0
				L117.state=0
			End If
		End If
	End If
End If
	If MudBogDefeated(CurrentPlayer) => 5 Then
		MudBog(CurrentPlayer) = 4
		MudBogDefeated(CurrentPlayer)= 0
		AddTimeForMission(CurrentPlayer) = 1
		NumberOfMissioncomplete(CurrentPlayer) = NumberOfMissioncomplete(CurrentPlayer) + 1
		NumberOfHitForStartMission(CurrentPlayer) = 0
		ShieldIsReadyToActivateMission(CurrentPlayer) = False
		ActionWhenHitOneOfThreeWay
		MusicCheck 22
		ModeTotalHydra.Interval = 5000 
		If ExtraBallsIsLit = True Then
			ModeTotalHydra.Interval = 11000 
		End If
		ModeTotalHydra.Enabled = True
	ElseIf MudBogDefeated(CurrentPlayer) = 2 Then
		KnightLamp(CurrentPlayer) = KnightLamp(CurrentPlayer) + 1
		If KnightLamp(CurrentPlayer) > 12 Then KnightLamp(CurrentPlayer) =12 End if
		Video_Knight
	Else
	End If
	If MoltenFireDefeated(CurrentPlayer) => 8 Then
		MoltenFire(CurrentPlayer) = 4
		MoltenFireDefeated(CurrentPlayer) = 0
		AddTimeForMission(CurrentPlayer) = 1
		NumberOfMissioncomplete(CurrentPlayer) = NumberOfMissioncomplete(CurrentPlayer) + 1
		NumberOfHitForStartMission(CurrentPlayer) = 0
		ShieldIsReadyToActivateMission(CurrentPlayer) = False
		ActionWhenHitOneOfThreeWay
		MusicCheck 22
		ModeTotalFireElement.Interval = 4000 
		If ExtraBallsIsLit = True Then
			ModeTotalFireElement.Interval = 10000 
		End If
		ModeTotalFireElement.Enabled = True
	ElseIf MoltenFireDefeated(CurrentPlayer) = 3 Then
		KnightLamp(CurrentPlayer) = KnightLamp(CurrentPlayer) + 1
		If KnightLamp(CurrentPlayer) > 12 Then KnightLamp(CurrentPlayer) =12 End if
		Video_Knight
	Else
	End If
	If BurningSandsDefeated(CurrentPlayer) => 6 Then
		BurningSands(CurrentPlayer) = 4
		BurningSandsDefeated(CurrentPlayer) = 0
		AddTimeForMission(CurrentPlayer) = 1
		NumberOfMissioncomplete(CurrentPlayer) = NumberOfMissioncomplete(CurrentPlayer) + 1
		NumberOfHitForStartMission(CurrentPlayer) = 0
		ShieldIsReadyToActivateMission(CurrentPlayer) = False
		ActionWhenHitOneOfThreeWay
		MusicCheck 22
		ModeTotalSandWorm.Interval = 4000 
		If ExtraBallsIsLit = True Then
			ModeTotalSandWorm.Interval = 10000 
		End If
		ModeTotalSandWorm.Enabled = True
	ElseIf BurningSandsDefeated(CurrentPlayer) = 3 Then
		KnightLamp(CurrentPlayer) = KnightLamp(CurrentPlayer) + 1
		If KnightLamp(CurrentPlayer) > 12 Then KnightLamp(CurrentPlayer) =12 End if
		Video_Knight
	Else
	End If
	If WickedCavernDefeated(CurrentPlayer) => 8 Then
		WickedCavern(CurrentPlayer) = 4
		WickedCavernDefeated(CurrentPlayer) = 0
		AddTimeForMission(CurrentPlayer) = 1
		NumberOfMissioncomplete(CurrentPlayer) = NumberOfMissioncomplete(CurrentPlayer) + 1
		NumberOfHitForStartMission(CurrentPlayer) = 0
		ShieldIsReadyToActivateMission(CurrentPlayer) = False
		ActionWhenHitOneOfThreeWay
		MusicCheck 22
		ModeTotalHandHolder.Interval = 4500 
		If ExtraBallsIsLit = True Then
			ModeTotalHandHolder.Interval = 10500 
		End If
		ModeTotalHandHolder.Enabled = True
	ElseIf WickedCavernDefeated(CurrentPlayer) = 3 Then
		KnightLamp(CurrentPlayer) = KnightLamp(CurrentPlayer) + 1
		If KnightLamp(CurrentPlayer) > 12 Then KnightLamp(CurrentPlayer) =12 End if
		Video_Knight
	Else
	End If
	If DeepFreezeDefeated(CurrentPlayer) => 4 Then
		DeepFreeze(CurrentPlayer) = 4
		DeepFreezeDefeated(CurrentPlayer) = 0
		AddTimeForMission(CurrentPlayer) = 1
		NumberOfMissioncomplete(CurrentPlayer) = NumberOfMissioncomplete(CurrentPlayer) + 1
		NumberOfHitForStartMission(CurrentPlayer) = 0
		ShieldIsReadyToActivateMission(CurrentPlayer) = False
		ActionWhenHitOneOfThreeWay
		MusicCheck 22
		ModeTotalLich.Interval = 4000 
		If ExtraBallsIsLit = True Then
			ModeTotalLich.Interval = 10000 
		End If
		ModeTotalLich.Enabled = True
		DeepFreezeMissionHitStart(CurrentPlayer) = False
	ElseIf DeepFreezeDefeated(CurrentPlayer) = 2 Then
		KnightLamp(CurrentPlayer) = KnightLamp(CurrentPlayer) + 1
		If KnightLamp(CurrentPlayer) > 12 Then KnightLamp(CurrentPlayer) =12 End if
		Video_Knight
	Else
	End If
	If BlackCastleDefeated(CurrentPlayer) => 5 Then
		BlackCastle(CurrentPlayer) = 4
		BlackCastleDefeated(CurrentPlayer) =0
		AddTimeForMission(CurrentPlayer) = 1
		NumberOfMissioncomplete(CurrentPlayer) = NumberOfMissioncomplete(CurrentPlayer) + 1
		NumberOfHitForStartMission(CurrentPlayer) = 0
		ShieldIsReadyToActivateMission(CurrentPlayer) = False
		ActionWhenHitOneOfThreeWay
		MusicCheck 22
		ModeTotalBlackCastle.Enabled = True
		BlackCastleMissionHitStart(CurrentPlayer) = False
	End If
'End If
'VideoSetBackground
End Sub

Sub ActionWhenHitOneOfThreeWay()								'SW56 , SW58+SW60, boule_hit+Kicker002

If NumberOfMissioncomplete(CurrentPlayer) = 0 Then						' * 1 -  Condition for Start the First Mission 
	If NumberOfHitForStartMission(CurrentPlayer) = 0 Then
		RampActivForValidateMission(CurrentPlayer) = False
		StopBouleAngle = 0
		StopRotating.Enabled=True
		RampLightsMove(CurrentPlayer) = "Blinking"

'		ShieldRemainsLock = True
'		Ramp02LightSequence
	End If
	If NumberOfHitForStartMission(CurrentPlayer) => 1 and CurrentMissionFlag(CurrentPlayer) = 0 Then
		RampActivForValidateMission(CurrentPlayer) = True
		If ShieldIsReadyToActivateMission(CurrentPlayer) = True Then
			RampLightsMove(CurrentPlayer) = "ShieldReadyForMonster"
			If BouleHited = 1 Then
				StopBouleAngle = 0
				StopRotating.interval=100
				StopRotating.Enabled=True		
				Shield_New_Status = "Unlock"
				ShieldTimer.Enabled=True
			End If
			If BouleHited = 2 Then
				StopBouleFlag = False
				StopRotating.Enabled=False
				timer2.interval=8
				timer2.Enabled=True
			End If
		Else
			BouleHited = 0
			StopBouleAngle = 90
			StopBouleFlag = True
			StopRotating.interval=100
			StopRotating.Enabled=True
			RampLightsMove(CurrentPlayer) = "UP"
		End If
		
'		Ramp02LightSequence
	End If
End If
If NumberOfMissioncomplete(CurrentPlayer) = 1 Then						' * 2 -  Condition for Start the Second Mission 
	If NumberOfHitForStartMission(CurrentPlayer) = 0 Then
		RampActivForValidateMission(CurrentPlayer) = False
		RampLightsMove(CurrentPlayer) = "Blinking"
'		Ramp02LightSequence
		StopBouleAngle = 0
		StopRotating.Enabled=True
		ShieldRemainsLock = True
	End If
	If NumberOfHitForStartMission(CurrentPlayer) = 1 Then
		StopBouleFlag = False
		StopRotating.Enabled=False
		timer2.interval=15
		timer2.Enabled=True
		ShieldNumberOfMove = 1
		ShieldTimer.interval=250
		ShieldTimer.Enabled=True
		ShieldRemainsLock = True

	End If
	If NumberOfHitForStartMission(CurrentPlayer) => 2 and CurrentMissionFlag(CurrentPlayer) = 0 Then
		ShieldRemainsLock = False
		RampActivForValidateMission(CurrentPlayer) = True
		If ShieldIsReadyToActivateMission(CurrentPlayer) = True Then
			RampLightsMove(CurrentPlayer) = "ShieldReadyForMonster"
			If BouleHited = 0 Then
				StopBouleAngle = 0
				StopRotating.interval=100
				StopRotating.Enabled=True		
				Shield_New_Status = "Unlock"
				ShieldTimer.Enabled=True
			End If
			If BouleHited = 1 Then
				StopBouleFlag = False
				StopRotating.Enabled=False
				timer2.interval=10
				timer2.Enabled=True
			End If		
		Else
			BouleHited = 0
			StopBouleAngle = 90
			StopBouleFlag = True
			StopRotating.interval=100
			StopRotating.Enabled=True
'			RampActivForValidateMission(CurrentPlayer) = True
			RampLightsMove(CurrentPlayer) = "UP"
'		Ramp02LightSequence
		End If
	End If
End If		
If NumberOfMissioncomplete(CurrentPlayer) = 2 Then						' * 3 -  Condition for Start the 3th Mission 
	If NumberOfHitForStartMission(CurrentPlayer) = 0 Then
		RampActivForValidateMission(CurrentPlayer) = False
		ShieldRemainsLock = True
		RampLightsMove(CurrentPlayer) = "Blinking"
'		Ramp02LightSequence
		StopBouleAngle = 0
		StopRotating.Enabled=True
	End If
	If NumberOfHitForStartMission(CurrentPlayer) = 1 Then
		ShieldRemainsLock = True
		StopBouleFlag = False
		StopRotating.Enabled=False
		timer2.interval=15
		timer2.Enabled=True

		shieldNumberOfMove = 1
		ShieldTimer.interval=250
		ShieldTimer.Enabled=True
		
	End If
	If NumberOfHitForStartMission(CurrentPlayer) = 2 Then
		ShieldRemainsLock = True
		StopBouleFlag = False
		StopRotating.Enabled=False
		timer2.interval=12
		timer2.Enabled=True
	End If
	If NumberOfHitForStartMission(CurrentPlayer) => 3 and CurrentMissionFlag(CurrentPlayer) = 0 Then
		ShieldRemainsLock = False
		RampActivForValidateMission(CurrentPlayer) = True
		If ShieldIsReadyToActivateMission(CurrentPlayer) = True Then
			RampLightsMove(CurrentPlayer) = "ShieldReadyForMonster"
			If BouleHited = 0 Then
				StopBouleAngle = 0
				StopRotating.interval=100
				StopRotating.Enabled=True		
				Shield_New_Status = "Unlock"
				ShieldTimer.Enabled=True
			End If
			If BouleHited = 1 Then
				StopBouleFlag = False
				StopRotating.Enabled=False
				timer2.interval=8
				timer2.Enabled=True
			End If
		Else
			BouleHited = 0
			StopBouleAngle = 90
			StopBouleFlag = True
			StopRotating.interval=100
			StopRotating.Enabled=True
'			RampActivForValidateMission(CurrentPlayer) = True
			RampLightsMove(CurrentPlayer) = "UP"
'			Ramp02LightSequence
		End If
	End If
End If
If NumberOfMissioncomplete(CurrentPlayer) = 3 Then						' * 4 -  Condition for Start the 4th Mission 
	If NumberOfHitForStartMission(CurrentPlayer) = 0 Then
		RampActivForValidateMission(CurrentPlayer) = False
		ShieldRemainsLock = True
		RampLightsMove(CurrentPlayer) = "Blinking"
'		Ramp02LightSequence
		StopBouleAngle = 0
		StopRotating.Enabled=True
	End If
	If NumberOfHitForStartMission(CurrentPlayer) = 1 Then
		ShieldRemainsLock = True
		StopBouleFlag = False
		StopRotating.Enabled=False
		timer2.interval=15
		timer2.Enabled=True
	End If
	If NumberOfHitForStartMission(CurrentPlayer) = 2 Then
		ShieldRemainsLock = True
		StopBouleFlag = False
		StopRotating.Enabled=False
		timer2.interval=12
		timer2.Enabled=True
	End If
	If NumberOfHitForStartMission(CurrentPlayer) = 3 Then
		ShieldRemainsLock = True
		StopBouleFlag = False
		StopRotating.Enabled=False
		timer2.interval=10
		timer2.Enabled=True
	End If
	If NumberOfHitForStartMission(CurrentPlayer) => 4 and CurrentMissionFlag(CurrentPlayer) = 0 Then
		ShieldRemainsLock = False
		RampActivForValidateMission(CurrentPlayer) = True
		If ShieldIsReadyToActivateMission(CurrentPlayer) = True Then
			RampLightsMove(CurrentPlayer) = "ShieldReadyForMonster"
			If BouleHited = 0 Then
				StopBouleAngle = 0
				StopRotating.interval=100
				StopRotating.Enabled=True		
				Shield_New_Status = "Unlock"
				ShieldTimer.Enabled=True
			End If
			If BouleHited = 1 Then
				StopBouleFlag = False
				StopRotating.Enabled=False
				timer2.interval=8
				timer2.Enabled=True
			End If		
		Else
			BouleHited = 0
			StopBouleAngle = 90
			StopBouleFlag = True
			StopRotating.interval=100
			StopRotating.Enabled=True
'			RampActivForValidateMission(CurrentPlayer) = True
			RampLightsMove(CurrentPlayer) = "UP"
'		Ramp02LightSequence
		End If
	End If
End If
If NumberOfMissioncomplete(CurrentPlayer) = 4 Then						' * 5 -  Condition for Start the 5th Mission 
	If NumberOfHitForStartMission(CurrentPlayer) = 0 Then
		RampActivForValidateMission(CurrentPlayer) = False
		ShieldRemainsLock = True
		RampLightsMove(CurrentPlayer) = "Blinking"
'		Ramp02LightSequence
		StopBouleAngle = 0
		StopRotating.Enabled=True
	End If
	If NumberOfHitForStartMission(CurrentPlayer) = 1 Then
		ShieldRemainsLock = True
		StopBouleFlag = False
		StopRotating.Enabled=False
		timer2.interval=15
		timer2.Enabled=True
	End If
	If NumberOfHitForStartMission(CurrentPlayer) = 2 Then
		ShieldRemainsLock = True
		StopBouleFlag = False
		StopRotating.Enabled=False
		timer2.interval=12
		timer2.Enabled=True
	End If
	If NumberOfHitForStartMission(CurrentPlayer) = 3 Then
		ShieldRemainsLock = True
		StopBouleFlag = False
		StopRotating.Enabled=False
		timer2.interval=9
		timer2.Enabled=True
	End If
	If NumberOfHitForStartMission(CurrentPlayer) = 4 Then
		ShieldRemainsLock = True
		StopBouleFlag = False
		StopRotating.Enabled=False
		timer2.interval=6
		timer2.Enabled=True
	End If
	If NumberOfHitForStartMission(CurrentPlayer) => 5 and CurrentMissionFlag(CurrentPlayer) = 0 Then
		RampActivForValidateMission(CurrentPlayer) = True
		ShieldRemainsLock = False
		If ShieldIsReadyToActivateMission(CurrentPlayer) = True Then
			RampLightsMove(CurrentPlayer) = "ShieldReadyForMonster"
			If BouleHited = 0 Then
				StopBouleAngle = 0
				StopRotating.interval=100
				StopRotating.Enabled=True		
				Shield_New_Status = "Unlock"
				ShieldTimer.Enabled=True
			End If
			If BouleHited = 1 Then
				StopBouleFlag = False
				StopRotating.Enabled=False
				timer2.interval=8
				timer2.Enabled=True
			End If
		Else
			BouleHited = 0
			StopBouleAngle = 90
			StopBouleFlag = True
			StopRotating.interval=100
			StopRotating.Enabled=True
'			RampActivForValidateMission(CurrentPlayer) = True
			RampLightsMove(CurrentPlayer) = "UP"
'		Ramp02LightSequence
		End If
	End If
End If
If NumberOfMissioncomplete(CurrentPlayer) = 5 Then						' * 6 -  Condition for Start the 6th Mission 
	If NumberOfHitForStartMission(CurrentPlayer) = 0 Then
		RampActivForValidateMission(CurrentPlayer) = False
		ShieldRemainsLock = True
		RampLightsMove(CurrentPlayer) = "Blinking"
'		Ramp02LightSequence
		StopBouleAngle = 0
		StopRotating.Enabled=True
	End If
	If NumberOfHitForStartMission(CurrentPlayer) = 1 Then
		ShieldRemainsLock = True
		StopBouleFlag = False
		StopRotating.Enabled=False
		timer2.interval=15
		timer2.Enabled=True
	End If
	If NumberOfHitForStartMission(CurrentPlayer) = 2 Then
		ShieldRemainsLock = True
		StopBouleFlag = False
		StopRotating.Enabled=False
		timer2.interval=12
		timer2.Enabled=True
	End If
	If NumberOfHitForStartMission(CurrentPlayer) = 3 Then
		ShieldRemainsLock = True
		StopBouleFlag = False
		StopRotating.Enabled=False
		timer2.interval=9
		timer2.Enabled=True
	End If
	If NumberOfHitForStartMission(CurrentPlayer) = 4 Then
		ShieldRemainsLock = True
		StopBouleFlag = False
		StopRotating.Enabled=False
		timer2.interval=6
		timer2.Enabled=True
	End If
	If NumberOfHitForStartMission(CurrentPlayer) = 5 Then
		ShieldRemainsLock = True
		StopBouleFlag = False
		StopRotating.Enabled=False
		timer2.interval=3
		timer2.Enabled=True
	End If
	If NumberOfHitForStartMission(CurrentPlayer) => 6 and CurrentMissionFlag(CurrentPlayer) = 0 Then
		ShieldRemainsLock = False
		RampActivForValidateMission(CurrentPlayer) = True
		If ShieldIsReadyToActivateMission(CurrentPlayer) = True Then
			RampLightsMove(CurrentPlayer) = "ShieldReadyForMonster"
			If BouleHited = 0 Then
				StopBouleAngle = 0
				StopRotating.interval=100
				StopRotating.Enabled=True		
				Shield_New_Status = "Unlock"
				ShieldTimer.Enabled=True
			End If
			If BouleHited = 1 Then
				StopBouleFlag = False
				StopRotating.Enabled=False
				timer2.interval=8
				timer2.Enabled=True
			End If
		Else
			BouleHited = 0
			StopBouleAngle = 90
			StopBouleFlag = True
			StopRotating.interval=100
			StopRotating.Enabled=True
			
			RampLightsMove(CurrentPlayer) = "UP"
'		Ramp02LightSequence
		End If		
	End If
End If

If NumberOfMissioncomplete(CurrentPlayer) => 6 Then	
	NumberOfMissioncomplete(CurrentPlayer) = 0		
End If	
'CheckLamp.Enabled = True
End Sub

Sub MissionRandomChoice()

End Sub

Sub SelectMission()
	Select Case Int(Rnd*5)+1
		Case 1 : MissionRandom = "MudBog":  : LightObjectColor(1).color = RGB(0, 255, 0) : LightObjectColor(1).colorfull = RGB(0, 255, 0) : LightObjectColor(1).state=2 : 'MudBog = 1
		Case 2 : MissionRandom = "MoltenFire": LightObjectColor(3).color = RGB(255, 50, 0) : LightObjectColor(3).colorfull = RGB(255, 50, 0) : LightObjectColor(3).state=2 : 'MoltenFire = 1
		Case 3 : MissionRandom = "BurningSands": LightObjectColor(5).color = RGB(255, 255, 0) : LightObjectColor(5).colorfull = RGB(255, 255, 0) : LightObjectColor(5).state=2 : 'BurningSands = 1
		Case 4 : MissionRandom = "WickedCavern": LightObjectColor(7).color = RGB(255, 0, 0) : LightObjectColor(7).colorfull = RGB(255, 0, 0) : LightObjectColor(7).state=2 : 'WickedCavern = 1
		Case 5 : MissionRandom = "DeepFreeze": LightObjectColor(9).color = RGB(0, 0, 255) : LightObjectColor(9).colorfull = RGB(0, 0, 255) : LightObjectColor(9).state=2 : 'DeepFreeze = 1
	End Select
	MissionCompleteCheck
End Sub


Sub MissionCompleteCheck()
	
	If CurrentMissionFlag(CurrentPlayer) = 1 Then
		'Nothing The mission is not finish
	ElseIf MudBog(CurrentPlayer) = 4 And MoltenFire(CurrentPlayer) = 4 And BurningSands(CurrentPlayer) = 4 And WickedCavern(CurrentPlayer) = 4 And DeepFreeze(CurrentPlayer) = 4 Then
		MissionRandom = "BlackCastle"
		LightObjectColor(11).color = RGB(255, 0, 255)
		LightObjectColor(11).colorfull = RGB(255, 0, 255)
		LightObjectColor(11).state=2
		BlackCastle(CurrentPlayer) = 1
'					End If
'				End If
'			End If
'		End If
'	End If
	ElseIf MissionRandom = "MudBog" Then 
		If MudBog(CurrentPlayer) = 4 Then
			SelectMission
		Else
			MudBog(CurrentPlayer) = 1
		End If
'	End If
	ElseIf MissionRandom = "MoltenFire" Then
		If MoltenFire(CurrentPlayer) = 4 Then
			SelectMission
		Else
			MoltenFire(CurrentPlayer) = 1
		End If
'	End If
	ElseIf MissionRandom = "BurningSands" Then
		If BurningSands(CurrentPlayer) = 4  Then
			SelectMission
		Else
			BurningSands(CurrentPlayer) = 1
		End If
'	End If
	ElseIf MissionRandom = "WickedCavern" Then
		If WickedCavern(CurrentPlayer) = 4  Then
			SelectMission
		Else
			WickedCavern(CurrentPlayer) = 1
		End If
'	End If

	ElseIf MissionRandom = "DeepFreeze" Then
		If DeepFreeze(CurrentPlayer) = 4 Then
			SelectMission
		Else
			DeepFreeze(CurrentPlayer) = 1
		End If
	Else
		'This Case is not possible (MissionRandom = 5 possibility and 6 when the 5 basic mission is completed) in case of select again mission
		SelectMission
	End If
End Sub


Sub MissionRandomTimer_Timer()
	MissionRandomTimer.Interval=30000
	MissionRandomTimer.Enabled = False
	TurnOffRoundLights
	If MudBog(CurrentPlayer) = 1 Then
		If MudBogDefeated(CurrentPlayer) = 0 Then
			MudBog(CurrentPlayer) = 0
		Else 
			MudBog(CurrentPlayer) = 3
		End If
	End If
	If MoltenFire(CurrentPlayer) = 1 Then
		If MoltenFireDefeated(CurrentPlayer) = 0 Then
			MoltenFire(CurrentPlayer) = 0
		Else 
			MoltenFire(CurrentPlayer) = 3
		End If
	End If
	If BurningSands(CurrentPlayer) = 1 Then
		If BurningSandsDefeated(CurrentPlayer) = 0 Then
			BurningSands(CurrentPlayer) = 0
		Else 
			BurningSands(CurrentPlayer) = 3
		End If
	End If
	If WickedCavern(CurrentPlayer) = 1 Then
		If WickedCavernDefeated(CurrentPlayer) = 0 Then
			WickedCavern(CurrentPlayer) = 0
		Else 
			WickedCavern(CurrentPlayer) = 3
		End If
	End If
	If DeepFreeze(CurrentPlayer) = 1 Then
		If DeepFreezeDefeated(CurrentPlayer) = 0 Then
			DeepFreeze(CurrentPlayer) = 0
		Else 
			DeepFreeze(CurrentPlayer) = 3
		End If
	End If
	If BlackCastle(CurrentPlayer) = 1 Then
		If BlackCastleDefeated(CurrentPlayer) = 0 Then
			BlackCastle(CurrentPlayer) = 0
		Else 
			BlackCastle(CurrentPlayer) = 3
		End If
	End If
	SelectMission
End Sub

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  PER PLAYER LIGHTS
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'

	setlightvalues
	sub setlightvalues								'Start Playing for all players
		RampChangeStatus(CurrentPlayer) = "OFF"
		dim i
		For i = 1 to 4
         OldState_L101(i) = 0 
         OldState_L103(i) = 0
         OldState_L106(i) = 0
         OldState_L11(i) = 0
         OldState_L110(i) = 2
         OldState_L111(i) = 2
         OldState_L112(i) = 2
         OldState_L115(i) = 0
         OldState_L116(i) = 0
         OldState_L117(i) = 0
         OldState_L12(i) = 0
         OldState_L13(i) = 0
         OldState_L131(i) = 0
         OldState_L132(i) = 0
         OldState_L133(i) = 0
 '        OldState_L14(i) = 0
         OldState_L15(i) = 0
         OldState_L16(i) = 0
         OldState_L163(i) = 0
         OldState_L165(i) = 0
         OldState_L166(i) = 0
         OldState_L170(i) = 0
         OldState_L171(i) = 0
         OldState_L172(i) = 0
         OldState_L18(i) = 0
         OldState_L19(i) = 0
         OldState_L20(i) = 0
         OldState_L21(i) = 0
         OldState_L22(i) = 0
         OldState_L23(i) = 0
         OldState_L24(i) = 0
'         OldState_L26(i) = 0
'         OldState_L29(i) = 0
'         OldState_L32(i) = 0
'         OldState_L35(i) = 0
'         OldState_L38(i) = 0
'         OldState_L41(i) = 0
'         OldState_L44(i) = 0
'         OldState_L47(i) = 0
'         OldState_L50(i) = 0
'         OldState_L53(i) = 0
'         OldState_L56(i) = 0
'         OldState_L59(i) = 0
         OldState_L63(i) = 0
         OldState_L64(i) = 0
         OldState_L66(i) = 0
         OldState_L69(i) = 0
         OldState_L70(i) = 0
         OldState_L72(i) = 0
         OldState_L75(i) = 0
         OldState_L79(i) = 2
         OldState_L82(i) = 0
         OldState_L83(i) = 0
		 OldState_L85(i) = 2
         OldState_L88(i) = 0
         OldState_L89(i) = 0
         OldState_L91(i) = 2
         OldState_L94(i) = 0
         OldState_L95(i) = 0
         OldState_L96(i) = 0
         OldState_L98(i) = 0
         OldState_Light026(i) = 0
         OldState_Light054(i) = 0
         OldState_Light055(i) = 0
         OldState_Light056(i) = 0
         OldState_Light057(i) = 0
         OldState_Light058(i) = 0
         OldState_Light059(i) = 0
         OldState_Light060(i) = 0
         OldState_Light061(i) = 0
         OldState_Light062(i) = 0
         OldState_Light063(i) = 0

		Next
	end Sub
	
	sub rememberlights
		If BallsOnPlayfield > 0 Then
			SaveStateLightturnlight
		End If
		SaveStateLight
	end Sub

	sub resetcolors

	end Sub

	sub relighttable
		If BallsRemaining(CurrentPlayer) = 3 Then
			if modescompleted(currentplayer) = 0 Then
'				t27.state = 1
				pt27(currentplayer) = 1
				pt29(CurrentPlayer) = 1
'				t29.state = 1
				pt32(CurrentPlayer) = 1
'				t32.state = 1
			end If
		end If
		resetcolors
'		mazecall = 0
	L270.State=1
	L271.State=1
	L272.State=1

	If OldState_L101(CurrentPlayer) = 1 Then L101.state = 1 
	If OldState_L103(CurrentPlayer) = 1 Then L103.state = 1
	If OldState_L106(CurrentPlayer) = 1 Then L106.state = 1
	If OldState_L11(CurrentPlayer) = 1 Then L11.state = 1
	If OldState_L110(CurrentPlayer) = 1 Then L110.state = 1
	If OldState_L111(CurrentPlayer) = 1 Then L111.state = 1
	If OldState_L112(CurrentPlayer) = 1 Then L112.state = 1
	If OldState_L115(CurrentPlayer) = 1 Then L115.state = 1
	If OldState_L116(CurrentPlayer) = 1 Then L116.state = 1
	If OldState_L117(CurrentPlayer) = 1 Then L117.state = 1
	If OldState_L12(CurrentPlayer) = 1 Then L12.state = 1
	If OldState_L13(CurrentPlayer) = 1 Then L13.state = 1
	If OldState_L131(CurrentPlayer) = 1 Then L131.state = 1
	If OldState_L132(CurrentPlayer) = 1 Then L132.state = 1
	If OldState_L133(CurrentPlayer) = 1 Then L133.state = 1
'	If OldState_L14(CurrentPlayer) = 1 Then L14.state = 1
	If OldState_L15(CurrentPlayer) = 1 Then L15.state = 1
	If OldState_L16(CurrentPlayer) = 1 Then L16.state = 1
	If OldState_L163(CurrentPlayer) = 1 Then L163.state = 1
	If OldState_L165(CurrentPlayer) = 1 Then L165.state = 1
	If OldState_L166(CurrentPlayer) = 1 Then L166.state = 1
	If OldState_L170(CurrentPlayer) = 1 Then L170.state = 1
	If OldState_L171(CurrentPlayer) = 1 Then L171.state = 1
	If OldState_L172(CurrentPlayer) = 1 Then L172.state = 1
	If OldState_L18(CurrentPlayer) = 1 Then L18.state = 1
	If OldState_L19(CurrentPlayer) = 1 Then L19.state = 1
	If OldState_L20(CurrentPlayer) = 1 Then L20.state = 1
	If OldState_L21(CurrentPlayer) = 1 Then L21.state = 1
	If OldState_L22(CurrentPlayer) = 1 Then L22.state = 1
	If OldState_L23(CurrentPlayer) = 1 Then L23.state = 1
	If OldState_L24(CurrentPlayer) = 1 Then L24.state = 1
'	If OldState_L26(CurrentPlayer) = 1 Then L26.state = 1
'	If OldState_L29(CurrentPlayer) = 1 Then L29.state = 1
'	If OldState_L32(CurrentPlayer) = 1 Then L32.state = 1
'	If OldState_L35(CurrentPlayer) = 1 Then L35.state = 1
'	If OldState_L38(CurrentPlayer) = 1 Then L38.state = 1
'	If OldState_L41(CurrentPlayer) = 1 Then L41.state = 1
'	If OldState_L44(CurrentPlayer) = 1 Then L44.state = 1
'	If OldState_L47(CurrentPlayer) = 1 Then L47.state = 1
'	If OldState_L50(CurrentPlayer) = 1 Then L50.state = 1
'	If OldState_L53(CurrentPlayer) = 1 Then L53.state = 1
'	If OldState_L56(CurrentPlayer) = 1 Then L56.state = 1
'	If OldState_L59(CurrentPlayer) = 1 Then L59.state = 1
	If OldState_L63(CurrentPlayer) = 1 Then L63.state = 1
	If OldState_L64(CurrentPlayer) = 1 Then L64.state = 1
	If OldState_L66(CurrentPlayer) = 1 Then L66.state = 1
	If OldState_L69(CurrentPlayer) = 1 Then L69.state = 1
	If OldState_L70(CurrentPlayer) = 1 Then L70.state = 1
	If OldState_L72(CurrentPlayer) = 1 Then L72.state = 1
	If OldState_L75(CurrentPlayer) = 1 Then L75.state = 1
	If OldState_L79(CurrentPlayer) = 1 Then L79.state = 1
	If OldState_L82(CurrentPlayer) = 1 Then L82.state = 1
	If OldState_L83(CurrentPlayer) = 1 Then L83.state = 1
	If OldState_L85(CurrentPlayer) = 1 Then L85.state = 1
	If OldState_L88(CurrentPlayer) = 1 Then L88.state = 1
	If OldState_L89(CurrentPlayer) = 1 Then L89.state = 1
	If OldState_L91(CurrentPlayer) = 1 Then L91.state = 1
	If OldState_L94(CurrentPlayer) = 1 Then L94.state = 1
	If OldState_L95(CurrentPlayer) = 1 Then L95.state = 1
	If OldState_L96(CurrentPlayer) = 1 Then L96.state = 1
	If OldState_L98(CurrentPlayer) = 1 Then L98.state = 1
	If OldState_Light026(CurrentPlayer) = 1 Then Light026.state = 1
	If OldState_Light054(CurrentPlayer) = 1 Then Light054.state = 1
	If OldState_Light055(CurrentPlayer) = 1 Then Light055.state = 1
	If OldState_Light056(CurrentPlayer) = 1 Then Light056.state = 1
	If OldState_Light057(CurrentPlayer) = 1 Then Light057.state = 1
	If OldState_Light058(CurrentPlayer) = 1 Then Light058.state = 1
	If OldState_Light059(CurrentPlayer) = 1 Then Light059.state = 1
	If OldState_Light060(CurrentPlayer) = 1 Then Light060.state = 1
	If OldState_Light061(CurrentPlayer) = 1 Then Light061.state = 1
	If OldState_Light062(CurrentPlayer) = 1 Then Light062.state = 1
	If OldState_Light063(CurrentPlayer) = 1 Then Light063.state = 1

	If OldState_L101(CurrentPlayer) = 2 Then L101.state = 2 
	If OldState_L103(CurrentPlayer) = 2 Then L103.state = 2
	If OldState_L106(CurrentPlayer) = 2 Then L106.state = 2
	If OldState_L11(CurrentPlayer) = 2 Then L11.state = 2
	If OldState_L110(CurrentPlayer) = 2 Then L110.state = 2
	If OldState_L111(CurrentPlayer) = 2 Then L111.state = 2
	If OldState_L112(CurrentPlayer) = 2 Then L112.state = 2
	If OldState_L115(CurrentPlayer) = 2 Then L115.state = 2
	If OldState_L116(CurrentPlayer) = 2 Then L116.state = 2
	If OldState_L117(CurrentPlayer) = 2 Then L117.state = 2
	If OldState_L12(CurrentPlayer) = 2 Then L12.state = 2
	If OldState_L13(CurrentPlayer) = 2 Then L13.state = 2
	If OldState_L131(CurrentPlayer) = 2 Then L131.state = 2
	If OldState_L132(CurrentPlayer) = 2 Then L132.state = 2
	If OldState_L133(CurrentPlayer) = 2 Then L133.state = 2
'	If OldState_L14(CurrentPlayer) = 2 Then L14.state = 2
	If OldState_L15(CurrentPlayer) = 2 Then L15.state = 2
	If OldState_L16(CurrentPlayer) = 2 Then L16.state = 2
	If OldState_L163(CurrentPlayer) = 2 Then L163.state = 2
	If OldState_L165(CurrentPlayer) = 2 Then L165.state = 2
	If OldState_L166(CurrentPlayer) = 2 Then L166.state = 2
	If OldState_L170(CurrentPlayer) = 2 Then L170.state = 2
	If OldState_L171(CurrentPlayer) = 2 Then L171.state = 2
	If OldState_L172(CurrentPlayer) = 2 Then L172.state = 2
	If OldState_L18(CurrentPlayer) = 2 Then L18.state = 2
	If OldState_L19(CurrentPlayer) = 2 Then L19.state = 2
	If OldState_L20(CurrentPlayer) = 2 Then L20.state = 2
	If OldState_L21(CurrentPlayer) = 2 Then L21.state = 2
	If OldState_L22(CurrentPlayer) = 2 Then L22.state = 2
	If OldState_L23(CurrentPlayer) = 2 Then L23.state = 2
	If OldState_L24(CurrentPlayer) = 2 Then L24.state = 2
'	If OldState_L26(CurrentPlayer) = 2 Then L26.state = 2
'	If OldState_L29(CurrentPlayer) = 2 Then L29.state = 2
'	If OldState_L32(CurrentPlayer) = 2 Then L32.state = 2
'	If OldState_L35(CurrentPlayer) = 2 Then L35.state = 2
'	If OldState_L38(CurrentPlayer) = 2 Then L38.state = 2
'	If OldState_L41(CurrentPlayer) = 2 Then L41.state = 2
'	If OldState_L44(CurrentPlayer) = 2 Then L44.state = 2
'	If OldState_L47(CurrentPlayer) = 2 Then L47.state = 2
'	If OldState_L50(CurrentPlayer) = 2 Then L50.state = 2
'	If OldState_L53(CurrentPlayer) = 2 Then L53.state = 2
'	If OldState_L56(CurrentPlayer) = 2 Then L56.state = 2
'	If OldState_L59(CurrentPlayer) = 2 Then L59.state = 2
	If OldState_L63(CurrentPlayer) = 2 Then L63.state = 2
	If OldState_L64(CurrentPlayer) = 2 Then L64.state = 2
	If OldState_L66(CurrentPlayer) = 2 Then L66.state = 2
	If OldState_L69(CurrentPlayer) = 2 Then L69.state = 2
	If OldState_L70(CurrentPlayer) = 2 Then L70.state = 2
	If OldState_L72(CurrentPlayer) = 2 Then L72.state = 2
	If OldState_L75(CurrentPlayer) = 2 Then L75.state = 2
	If OldState_L79(CurrentPlayer) = 2 Then L79.state = 2
	If OldState_L82(CurrentPlayer) = 2 Then L82.state = 2
	If OldState_L83(CurrentPlayer) = 2 Then L83.state = 2
	If OldState_L85(CurrentPlayer) = 2 Then L85.state = 2
	If OldState_L88(CurrentPlayer) = 2 Then L88.state = 2
	If OldState_L89(CurrentPlayer) = 2 Then L89.state = 2
	If OldState_L91(CurrentPlayer) = 2 Then L91.state = 2
	If OldState_L94(CurrentPlayer) = 2 Then L94.state = 2
	If OldState_L95(CurrentPlayer) = 2 Then L95.state = 2
	If OldState_L96(CurrentPlayer) = 2 Then L96.state = 2
	If OldState_L98(CurrentPlayer) = 2 Then L98.state = 2
	If OldState_Light026(CurrentPlayer) = 2 Then Light026.state = 2
	If OldState_Light054(CurrentPlayer) = 2 Then Light054.state = 2
	If OldState_Light055(CurrentPlayer) = 2 Then Light055.state = 2
	If OldState_Light056(CurrentPlayer) = 2 Then Light056.state = 2
	If OldState_Light057(CurrentPlayer) = 2 Then Light057.state = 2
	If OldState_Light058(CurrentPlayer) = 2 Then Light058.state = 2
	If OldState_Light059(CurrentPlayer) = 2 Then Light059.state = 2
	If OldState_Light060(CurrentPlayer) = 2 Then Light060.state = 2
	If OldState_Light061(CurrentPlayer) = 2 Then Light061.state = 2
	If OldState_Light062(CurrentPlayer) = 2 Then Light062.state = 2
	If OldState_Light063(CurrentPlayer) = 2 Then Light063.state = 2


		currentplayerbackglass

		Select Case totalcombo(CurrentPlayer)
			case 10
				wandlockready
				'PuPlayer.playlistplayex pCallouts,"audiocallouts","wand lock ready.mp3",vovol,1
				playmedia "wand lock ready.mp3","audiocallouts",pCallouts,"",0,"",1,1  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'				pupDMDDisplay "-","Wand Lock^is Lit",dmdnote,3,0,10
			case 11
				totalcombo(CurrentPlayer) = 9
				wandlockready
		end Select
	End Sub





'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  SKILLSHOT & BALL SAVE
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' 
Sub StartBlackKnightRetroMulti_timer
		StartBlackKnightRetroMulti.Enabled=False
		Knightlamp(CurrentPlayer) = 0
		'CurrentMissionTimer.Enabled=true
		AddTimeForMission(CurrentPlayer) = MissionTime
		CurrentMissionFlag(CurrentPlayer) = 2
		horloge.Interval=1000
		horloge.Enabled = True
		kickball
'		lastmb = "StartBlackKnightRetroMulti '"
'		stopmer.collidable = 1
'		wandclose
'		indragon = 1
		bMultiBallMode = True
		doublekick.Enabled=True
		'AddMultiball 2
		bBallSaverActive = False
		EnableBallSaver 20:bBallSaverActive = True

end sub

	Sub launchend_hit
'		hbreturn.enabled = 1
		StopSound "fx_metalrolling"
		PlaySoundAt "fx_ball_drop1", launchend
	End sub

	Sub turnskillon_hit
		if bballfirstball = 1 Then
			skillshotlive = 1
			startskill
		end If
	End Sub


	Sub startskill
		StartRainbow skillshotlights
		dim a
		for each a in skillshotlights
			a.state = 2
		next
	End Sub

	Sub stopskill
		SkillshotValue = 1000000
		StopRainbow skillshotlights
		dim a
		for each a in skillshotlights
			a.state = 0
			SetLightColor a, green, -1
		next
	End Sub

	SkillshotValue = 1000000
	Sub AwardSkillshot
		Dim i
'		DOF 125, DOFPulse
		'GiEffect 1
		'LightEffect 2
		'LightEffect 9
'		DOF 600, DOFPulse   'DOF MX - Skillshot
		If SkillshotValue = 1000000 then
			'PuPlayer.playlistplayex pCallouts,"audiocallouts","skillshot.mp3",vovol,1
			playmedia "skillshot.mp3","audiocallouts",pCallouts,"",0,"",1,1  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'			pupDMDDisplay "-","Skillshot^1,000,000",dmdnote,3,0,10
			'***** LIGHT RUNS *****
			'(CHAOS)randoms
			'(DIRECTIONS)uup|udown|uleft|uright|diagdl|diagdr|diagul|diagur
			'(SWIPES)middleih|middleiv|middleoh|middleov|stripe1h|hatch1h|hatch1v|hatch2h|hatch2v|stripe1v|stripe2h|stripe2v
			'(SPINS)circlein|circleout|clockleft|clockright|screwl|screwr
			'(CURVES)arcbld|arcblu|arcbrd|arcbru|arctld|arctlu|arctrd|arctru|fanld|fanlu|fanrd|fanru|radarl|radarr|wiperl|wiperr
			'(EXAMPLE)lightrun red,arcbld,1  (SYNTAX)lightrun color,direction,times to run
			lightrun amber,screwl, 2
			flasherspop amber,"crazy"
			'chilloutthemusic
			AddScore 1000000
			skillshotvalue = skillshotvalue + 1000000
		Else
			'PuPlayer.playlistplayex pCallouts,"audiocallouts","super skillshot.mp3",vovol,1
			playmedia "super skillshot.mp3","audiocallouts",pCallouts,"",0,"",1,1  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'			pupDMDDisplay "-","Super^Skillshot",dmdnote,3,0,10
			lightrun amber,screwl, 3
			flasherspop amber,"crazy"
			'chilloutthemusic
			addscore 2000000
		End If
	End Sub


	Sub ballsavestarttrigger_hit
'		If skillshotlive = 1 Then
'			stopskill
'			dlcall = 0
'			potioncall = 0
'		end if 
'		skillshotlive = 0
'		bballfirstball = 0
'		If(bBallSaverReady = True) AND(15 <> 0) And(bBallSaverActive = False) Then
'			EnableBallSaver 15:bBallSaverActive = True
'		End If
	End Sub



	Sub EnableBallSaver(seconds)
		bBallSaverActive = True
		bBallSaverReady = False
		BallSaverTimerExpired.Interval = 1000 * seconds
		BallSaverTimerExpired.Enabled = True
'		BallSaverSpeedUpTimer.Interval = 1000 * seconds -(1000 * seconds) / 3
'		BallSaverSpeedUpTimer.Enabled = True
		' if you have a ball saver light you might want to turn it on at this point (or make it flash)

	End Sub

	' The ball saver timer has expired.  Turn it off AND reset the game flag
	'
	Sub BallSaverTimerExpired_Timer()

		BallSaverTimerExpired.Enabled = False
		' clear the flag
		Dim waittime
		waittime = 4000
		'vpmtimer.addtimer waittime, "ballsavegrace'"
		ballsavegrace
		' if you have a ball saver light then turn it off at this point
		If bExtraBallWonThisBall = True Then
'			L14.State = 1
			'
			'
			'Add ExtraBall
			'
			'
		Else
			bBallSaverActive = False
			If ExtraBallsAwards(CurrentPlayer) = 0 Then
				L14.State = 0
			End If
		End If
	End Sub

	Sub ballsavegrace
		dim blcalc, balnums
		blcalc = getballs
		balnums = UBound(blcalc) - 1
		dim mlnum:mlnum = merlock1full + merlock3full + merlock2full
		dim finballs:finballs = balnums - mlnum
'		debug.print "live balls:" &	finballs
'		BallsOnPlayfield = finballs
'		debug.print "Ballsaver ended"
		bBallSaverActive = False
		If ExtraBallsAwards(CurrentPlayer) = 0 Then
			L14.State = 0
		End If
	End Sub

	Sub quickcount
		dim blcalc, balnums
		blcalc = getballs
		balnums = UBound(blcalc) - 1
		dim mlnum:mlnum = merlock1full + merlock3full + merlock2full
		dim finballs:finballs = balnums - mlnum
'		debug.print "live balls:" &	finballs
'		BallsOnPlayfield = finballs
	End Sub

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  TARGET HIT EVENTS
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' 
Sub Ramp001_hit()
	playsound "fx_metalrolling"
End Sub

Sub Gate002Touch
BallControlTimer.Enabled = True
If LastChanceStart = True and LastChanceChronoStart(CurrentPlayer) = False Then
	CurrentMissionFlag(CurrentPlayer) = 3
	horloge.Enabled = True
End If
playsound "gate"
CheckCurrentMissionPlayer
StopBallControlFlag = False
LastChanceButtonPush(CurrentPlayer) = False
CheckLamp.Interval = 100
CheckLamp.Enabled = True
If skillshotlive = 1 Then
		stopskill
		dlcall = 0
		potioncall = 0
	end if 
	skillshotlive = 0
	bballfirstball = 0
	If(bBallSaverReady = True) AND(15 <> 0) And(bBallSaverActive = False) Then
		EnableBallSaver 15:bBallSaverActive = True
	End If
													'Hitting the gate is the trigger to start the skill shot timer
 	'AddMultiball 1
	playsound "fx_ball_drop1"
	If BallsOnPlayfield=0 Then 
		If Musicball < Ball Then
			Musicball = Ball
			'Table1_MusicDone 
		End if
	End If												'Stop music and random music
	If NewBallInLock = True Then
		BallsOnPlayfield = BallsOnPlayfield
	Else
		BallsOnPlayfield = BallsOnPlayfield + 1
	End If
														'The Gate is not refered I Used the SWITCH number 86 not used in the documentation
	If BallInPlay=false and AutoFireTimer.Enabled=false then		'Make sure that no ball is alreay in play & that the ball saver auto fire is not in use
 		Plunger.TimerEnabled=true									'Turns the plunger timer on - this will be used to time the skill shot
 		BallInPlay=true												'Flag to show ball has entered play
		BallSaverTrigger_hit
		'SkillShotTimerLight.State=1									'Diagnostic light to show skill shot timer is active
 	End If
	shotskill.Enabled=True
	shotskillflag = True
	'SWNumber = 86
	'StartMusicCheck 86
	MusicCheck 86
End Sub

Sub Gate02_hit()
	Gate002Touch
End Sub

Sub Gate002_hit()
	Gate002Touch
End Sub
Sub Gate003_hit()
	AddScore 250000
	BallInLocker = BallInLocker + 1
	BallInCatapult(CurrentPlayer) = BallInCatapult(CurrentPlayer) + 1
	BallsOnPlayfield = BallsOnPlayfield - 1
	If catapult_lock_is_lit(CurrentPlayer) = False or BlackKnightRetro(CurrentPlayer) = 1 or BlackKnightRetro(CurrentPlayer) = 3 or CatapultModeFlag = True Then
		If LastChanceCatapultMode = False Then
			UPLock.collidable=False
			UPLock.TransY=-20
		Else
			UPLock.collidable=True
			UPLock.TransY=0	
			LastChanceBallInCatapult.interval = 35000
			LastChanceBallInCatapult.Enabled = True 
		End If
	Else
		'AddscoreCatapult = AddscoreCatapult + 500000
		UPLock.collidable=True
		UPLock.TransY=0	
		If shotskillflag = true And LastChanceStart = False Then
			AddScore 2000000
			videoshotskill
			ShootToSkillTimer.Interval=5000
			ShootToSkillTimer.enabled=True
		Else
			ShootToSkillTimer.Interval=100
			ShootToSkillTimer.enabled=True
		End If
		
	End If
	If CatapultModeFlag = True Then
		
		Catapult_jackpot_collected = Catapult_jackpot_collected * Catapult_Super_Jackpot_Multi
		Catapult_Mode_Total_score(CurrentPlayer) = Catapult_Mode_Total_score(CurrentPlayer) + Catapult_jackpot_collected
		AddScore Catapult_jackpot_collected
		If Catapult_Super_Jackpot_Multi = 1 Then
			CommentDisplayed = "Catapult Super Jackpot!"
			Select Case Int(Rnd*3)+1
				Case 1 : audioknight = "Sound-0x01F7.mp3":SpeakTime = 3168
				Case 2 : audioknight = "Sound-0x01F7.mp3":SpeakTime = 3168
				Case 3 : audioknight = "Sound-0x01F7.mp3":SpeakTime = 1000
			End Select
		ElseIf Catapult_Super_Jackpot_Multi = 2 Then
			CommentDisplayed = "2x Catapult Super Jackpot!"
			Select Case Int(Rnd*3)+1
				Case 1 : audioknight = "Sound-0x01F7.mp3":SpeakTime = 3168
				Case 2 : audioknight = "Sound-0x01F7.mp3":SpeakTime = 3168
				Case 3 : audioknight = "Sound-0x01F7.mp3":SpeakTime = 3168
			End Select
		ELseIf Catapult_Super_Jackpot_Multi = 3 Then
			CommentDisplayed = "3x Catapult Super Jackpot!"
			Select Case Int(Rnd*3)+1
				Case 1 : audioknight = "Sound-0x01F7.mp3":SpeakTime = 3168
				Case 2 : audioknight = "Sound-0x01F7.mp3":SpeakTime = 3168
				Case 3 : audioknight = "Sound-0x01F7.mp3":SpeakTime = 3168
			End Select
		Else
			CommentDisplayed = "4x Catapult Super Jackpot!"
			Select Case Int(Rnd*3)+1
				Case 1 : audioknight = "Sound-0x01F7.mp3":SpeakTime = 3168
				Case 2 : audioknight = "Sound-0x01F7.mp3":SpeakTime = 3168
				Case 3 : audioknight = "Sound-0x01F7.mp3":SpeakTime = 3168
			End Select
		End If
		Video_Catapult_Multball_jacpot
		LightEyesBK
		playmedia audioknight,"Audioknight",pAudio,"",0,"",1,1  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	End If
	
End Sub

Sub LastChanceBallInCatapult_timer()
	LastChanceBallInCatapult.Enabled = False
	BallKeptInLocker =  0
	'	BallInCatapult(CurrentPlayer) = 0
	LockerTimer.Enabled=True
	Catapult_Super_Jackpot_Multi = 1
End Sub

Sub CatapultManagement_Timer()
If BallInLocker = 1 Then									''**************** BALL IN LOCKER 1 ******************
	If BallInCatapult(CurrentPlayer) = 1 Then
		BallKeptInLocker = 1
		RequestANewBall = True
		'PlaySound "Sound-0x0910"
'		video_catapult.enabled= True
		L131.state=1
		L132.state=2
		L133.state=0
	End If
	If BallInCatapult(CurrentPlayer) = 2 Then
		RequestANewBall = True
		BallKeptInLocker = 1
		'PlaySound "Sound-0x08F6"
'		video_catapult.enabled= True
		L131.state=1
		L132.state=1
		L133.state=2
	End If

	If BallInCatapult(CurrentPlayer) = 3 Then
		RequestANewBall = False
		LockerTimer.Enabled=True
'		video_catapult.enabled= True
		L131.state=1
		L132.state=1
		L133.state=1
		BallKeptInLocker =  0
'		BallInCatapult(CurrentPlayer) = 0
'		TEST
		kickball							'Ball in Locker 1
		bMultiBallMode = True				'Ball in Locker 1
		doublekick.Enabled=True				'Ball in Locker 1
		CatapultModeFlag = True
		BouleAfterHit.Enabled = True
		Catapult_Super_Jackpot_Multi = 1
		MusicCheck 85
'		EnableBallSaver 40:bBallSaverActive = True
'		If CurrentMissionFlag(CurrentPlayer) = 0 Then
'			video_catapult_mode_loop
'		End If
	End If
End If

If BallInLocker = 2 Then									''**************** BALL IN LOCKER 2 ******************
	AddScore 250000
	If BallInCatapult(CurrentPlayer) = 1 Then
		RequestANewBall = False
		'PlaySound "Sound-0x0910"
'		video_catapult.enabled= True
		L131.state=1
		L132.state=2
		L133.state=0
		BallKeptInLocker = 1
		LockerTimer.Enabled=True
	End If
	If BallInCatapult(CurrentPlayer) = 2 Then
		RequestANewBall = true
		BallKeptInLocker = 2
		'BallKeptInLocker = 2
		'PlaySound "Sound-0x08F6"
'		video_catapult.enabled= True
		L131.state=1
		L132.state=1
		L133.state=2
	End If
	If BallInCatapult(CurrentPlayer) = 3 Then
		RequestANewBall = False
		LockerTimer.Enabled=True
'		video_catapult.enabled= True
		L131.state=1
		L132.state=1
		L133.state=1
		BallKeptInLocker =  0
'		BallInCatapult(CurrentPlayer) = 0
'		TEST
		kickball								'Ball in Locker 2
		bMultiBallMode = True					'Ball in Locker 2
		CatapultModeFlag = True
		BouleAfterHit.Enabled = True
		Catapult_Super_Jackpot_Multi = 1
		MusicCheck 85
'		EnableBallSaver 40:bBallSaverActive = True
'		If CurrentMissionFlag(CurrentPlayer) = 0 Then
'			video_catapult_mode_loop
'		End If
	End If
End If

If BallInLocker = 3 Then							'**************** BALL IN LOCKER 3 ******************
	AddScore 100000
	If BallInCatapult(CurrentPlayer) = 1 Then
		'BallKeptInLocker = 1
		RequestANewBall = False
		'PlaySound "Sound-0x0910"
'		video_catapult.enabled= True
		L131.state=1
		L132.state=2
		L133.state=0
		BallKeptInLocker = 2
		LockerTimer.Enabled=True
	End If
	If BallInCatapult(CurrentPlayer) = 2 Then
		RequestANewBall = False
		'BallKeptInLocker = 2
		'PlaySound "Sound-0x08F6"
'		video_catapult.enabled= True
		L131.state=1
		L132.state=1
		L133.state=2
		BallKeptInLocker = 2
		LockerTimer.Enabled=True
	End If
	If BallInCatapult(CurrentPlayer) = 3 Then
		RequestANewBall = False
		LockerTimer.Enabled=True		
'		video_catapult.enabled= True
		L131.state=1
		L132.state=1
		L133.state=1
		BallKeptInLocker =  0
'		BallInCatapult(CurrentPlayer) = 0
'		TEST
		CatapultModeFlag = True
		BouleAfterHit.Enabled = True
		Catapult_Super_Jackpot_Multi = 1
		MusicCheck 85
'		EnableBallSaver 40:bBallSaverActive = True
'		If CurrentMissionFlag(CurrentPlayer) = 0 Then
'			video_catapult_mode_loop
'		End If
	End If
End If

If RequestANewBall = True Then
	If BallsOnPlayfield = 0 Then
		Kicker1.CreateBall
		'Kicker1.Kick 90,10
		Kicker1.Kick 300, 30, 1.56
		playsound SoundFXDOF("ballrelease", 110, DOFPulse, DOFContactors)
	Else
		AutoPlunger.createball
		AutoPlunger.kick 360,45
		playsound SoundFXDOF("ballrelease", 110, DOFPulse, DOFContactors)
	End If
End If
CatapultManagement.Enabled = False
End Sub

Sub ShootToSkillTimer_Timer()
	ShootToSkillTimer.enabled=False
	If BallInCatapult(CurrentPlayer) = 1 Then
		playmedia "Sound-0x0910.mp3","Audiocomment",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		If LastChanceStart = False Then
			VideocatapultBall1lock
			CatapultManagement.interval = 2000
			CatapultManagement.Enabled = True
		Else
			VideoSupercatapultBall1lock
		End If
	ElseIf BallInCatapult(CurrentPlayer) = 2 Then
		playmedia "Sound-0x08F6.mp3","Audiocomment",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		If LastChanceStart = False Then
			VideocatapultBall2lock
			CatapultManagement.interval = 2000
			CatapultManagement.Enabled = True
		Else
			VideoSupercatapultBall2lock
		End If
	ElseIf BallInCatapult(CurrentPlayer) = 3 Then
		CatapultModeFlag = True
		BouleAfterHit.Enabled = True
		playmedia "Sound-0x0858.mp3","Audiocomment",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		If LastChanceStart = False Then
			VideocatapultBall3lock
		Else
			horloge.Interval=100
			LastChanceSuccess = True
			DisableTable True					'In test
			VideoSupercatapultBall3lock
		End If
	Else
		' This Case is not previous, because not possible
	End If
End Sub

Sub video_catapult_Timer()
'	video_catapult.Interval= 4000
'
'	video_catapult.enabled= False
End Sub

Sub Gate004_hit()
	UPLock.collidable=True
	UPLock.TransY=0
End Sub

Sub Gate005_hit()
	BallInLocker = BallInLocker - 1
	BallsOnPlayfield = BallsOnPlayfield + 1
	If BlackKnightRetro(CurrentPlayer) = 1 or BlackKnightRetro(CurrentPlayer) = 3 Then
		BallInCatapult(CurrentPlayer) = BallInCatapult(CurrentPlayer) - 1
	End If
	If BallInLocker = BallKeptInLocker Then
		UPLock.collidable=True
		UPLock.TransY=0
		LockerTimer.Enabled=False
		If BallKeptInLocker = 0 Then
			catapult_lock_is_lit(CurrentPlayer) = False
			BallInCatapult(CurrentPlayer) = 0
		End If
		L131.state=0
		L132.state=0
		L133.state=0
	Else 
		LockerTimer.Enabled=True
		'UPLock.collidable=False
		'UPLock.TransY=-20
	End If
End Sub

Sub LockerTimer_Timer()
	UPLock.collidable=False
	UPLock.TransY=-20
	LockerTimer.Enabled=False
	BallInCatapult(CurrentPlayer) = BallInCatapult(CurrentPlayer) - 1
	If RequestANewBall = False And BallKeptInLocker <> 0 Then
		'BallInCatapult(CurrentPlayer) = BallKeptInLocker
		BallInCatapult(CurrentPlayer) = BallInCatapult(CurrentPlayer) + 1
	End If
End Sub

Sub SkillShotTarget_hit
 	If Plunger.TimerEnabled=true then SkillShotTarget.IsDropped=true	'Diagnostic just to show for this table that the skill shot was made in time
 	Plunger.TimerEnabled=false										'Turn off the timer
' 	SkillShotTimerLight.State=0										'Diagnostic light to show skill shot timer is inactive
End Sub

Sub Plunger_Timer()
 	Plunger.TimerEnabled=false										'Turns off the timer
' 	SkillShotTimerLight.State=0										'Diagnostic light to show skill shot timer is inactive
End Sub

				  
				 
	   



Sub shotskill_Timer()
	shotskillflag = False
	shotskill.Enabled=false	
End Sub
'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

'++++++++++++++++++++++++++ BALL SAVER TIMER ++++++++++++++++++++++++++++++++++++++
'The ball saver timing starts when the Ball Saver Trigger is hit (in a real game I would set this timer going when the ball hits the gate
'	i.e. at the same time as the skill shot timer, but in this case I will use a trigger to start it)
'	and it is only started provided the ball saver has not just been used (AutoFireTimer)
'It ends when the drain is hit, or when the timer interval is reached (set in Table1_init)
'	If the drain is hit the timer uses another timer to automatically fire a new ball into play (AutoFireTimer)
 
Sub BallSaverTrigger_hit()
 	If AutoFireTimer.Enabled=false then								'Check to see that the AutoFire is not in use
' 		L14.State=2
 '		BallSaverTimerLight.State=1									'Diagnostic light to show ball saver timer is active
 		BallSaverTimerExpired.Enabled=true									'Turn ball save timer on

 	End If
End Sub

'Sub BallSaverTimer_Timer()
' 	BallSaverTimer.Enabled=false									'Turns off the timer
' 	L14.State=0
'' 	BallSaverTimerLight.State=0										'Diagnostic light to show ball saver timer is inactive
'End Sub

Sub AutoFireTimer_Timer()											'Creates & fires a new ball automatically
	If BallsOnPlayfield < 7 Then
		'bBallSaverActive = False
		AutoFireTime=AutoFireTime+1										'Allows script to run the timer several times with different events for each cycle
		Select Case AutoFireTime
			Case 1														'First count pulls back the plunger
				Plunger.PullSpeed=500
				Plunger.PullBack
			Case 2
				'Plunger.CreateBall										'Second count creates a ball & fires it
				AutoPlunger.createball
				AutoPlunger.kick 360,45
				playsound "ballrelease"
				Plunger.Fire
				playsound "popper_ball"
			Case 5														'Fifth count turns off timer & resets variables
				AutoFireTimer.Enabled=false
				AutoFireTime=0											'Resets count for this timer so that the next time it runs it starts from the begining again
				Plunger.PullSpeed=5										'Resets the plunger pull back speed for player use
'				AutoFireTimerLight.State=0								'Diagnostic light to show auto fire timer is inactive
		End Select
	End If
End Sub


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  SECONDARY HIT EVENTS
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' 



'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  MODES
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' 




Sub ShieldTimer_Timer()
	If Shield_New_Status = "Unlock" Then
		Shield_lock = True
		ShieldTimer.Enabled=False
		DOF 112, DOFPulse
		DOF 605, DOFPulse
		pupevent 505
	End If
	If Shield_New_Status = "lock" Then
		Shield_lock = False
		ShieldTimer.Enabled=False
		DOF 118, DOFPulse
	End If
	If Shield_lock = False Then
		Shield_lock=True
		SW58.TransZ=0
		SW58.TransY=0
		SW58.ObjRotX=0
		SW58.Collidable=True
		ishield = ishield + 1
		If ShieldNumberOfMove < ishield Then	
			ShieldTimer.Enabled=False
			'ShieldNumberOfMove = 1
		End If
		'Target009.DropSol_On=0
	Else
		Shield_lock=False
		SW58.TransZ=-100
		SW58.TransY=50
		SW58.ObjRotX=20
		SW58.Collidable=False
		PlaySound "fx_bumper4"
		'Target009.DropSol_On=1
	End If
End Sub


Sub GoToInitVideo_Timer()
	GoToInitVideo.Enabled=False
	'CurrentMissionTimer.Enabled=False
	horloge.Enabled=False
	MissionRandomTimer.Enabled=False
'	video_init
End Sub

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  BUMPERS
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  

sub SW1_hit() 'R*** in (RAGE)
	If SuperLanes = True Then
		AddScoreChestJackpot = AddScoreChestJackpot + AddScoreSuperLanes
	End If
	If RetroMode = 1 or RetroMode = 3 Then
		playmedia "Sound-0x0113.mp3","Audionoise",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)

	Else
		'normal mode
		If BallSaveAvailable(CurrentPlayer) = True then								'Check to see if ball saver is active
'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
'			video_SavedBall					Used in Sub DrainerCheck
'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			L13.state = 1
			bBallSaverActiveL13 = True
		End If
'		addscore 1000
	End If
	addscore 1030
	BaseBonus = BaseBonus + 1800
	L11.state=1
	If L11.state=1 And L12.state=0 And L15.state=0 And L16.state=0  Then 'R***
		DOF 220, DOFPulse
	ElseIf L11.state=0 And L12.state=1 And L15.state=0 And L16.state=0  Then '*A**
		DOF 221, DOFPulse
	ElseIf L11.state=0 And L12.state=0 And L15.state=1 And L16.state=0  Then '**G*
		DOF 222, DOFPulse
	ElseIf L11.state=0 And L12.state=0 And L15.state=0 And L16.state=1  Then '***E
		DOF 223, DOFPulse
	ElseIf L11.state=1 And L12.state=1 And L15.state=0 And L16.state=0  Then 'RA**
		DOF 224, DOFPulse	
	ElseIf L11.state=1 And L12.state=0 And L15.state=1 And L16.state=0  Then 'R*G*
		DOF 225, DOFPulse
	ElseIf L11.state=1 And L12.state=0 And L15.state=0 And L16.state=1  Then 'R**E
		DOF 226, DOFPulse
	ElseIf L11.state=1 And L12.state=1 And L15.state=1 And L16.state=0  Then 'RAG*
		DOF 227, DOFPulse	
	ElseIf L11.state=1 And L12.state=0 And L15.state=1 And L16.state=1  Then 'R*GE
		DOF 228, DOFPulse	
	ElseIf L11.state=0 And L12.state=1 And L15.state=1 And L16.state=0  Then '*AG*
		DOF 229, DOFPulse	
	ElseIf L11.state=0 And L12.state=1 And L15.state=1 And L16.state=1  Then '*AGE
		DOF 230, DOFPulse	
	ElseIf L11.state=0 And L12.state=0 And L15.state=1 And L16.state=1  Then '**GE
		DOF 231, DOFPulse
	End If
end Sub

sub SW2_hit() '*A** in (RAGE)
	If SuperLanes = True Then
		AddScoreChestJackpot = AddScoreChestJackpot + AddScoreSuperLanes
	End If
	If RetroMode = 1 or RetroMode = 3 Then
		playmedia "Sound-0x0113.mp3","Audionoise",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	Else
		'normal mode
'		addscore 1000
	End If	
	addscore 1030
	BaseBonus = BaseBonus + 1800
	L12.state=1
	If L11.state=1 And L12.state=0 And L15.state=0 And L16.state=0  Then 'R***
		DOF 220, DOFPulse
	ElseIf L11.state=0 And L12.state=1 And L15.state=0 And L16.state=0  Then '*A**
		DOF 221, DOFPulse
	ElseIf L11.state=0 And L12.state=0 And L15.state=1 And L16.state=0  Then '**G*
		DOF 222, DOFPulse
	ElseIf L11.state=0 And L12.state=0 And L15.state=0 And L16.state=1  Then '***E
		DOF 223, DOFPulse
	ElseIf L11.state=1 And L12.state=1 And L15.state=0 And L16.state=0  Then 'RA**
		DOF 224, DOFPulse	
	ElseIf L11.state=1 And L12.state=0 And L15.state=1 And L16.state=0  Then 'R*G*
		DOF 225, DOFPulse
	ElseIf L11.state=1 And L12.state=0 And L15.state=0 And L16.state=1  Then 'R**E
		DOF 226, DOFPulse
	ElseIf L11.state=1 And L12.state=1 And L15.state=1 And L16.state=0  Then 'RAG*
		DOF 227, DOFPulse	
	ElseIf L11.state=1 And L12.state=0 And L15.state=1 And L16.state=1  Then 'R*GE
		DOF 228, DOFPulse	
	ElseIf L11.state=0 And L12.state=1 And L15.state=1 And L16.state=0  Then '*AG*
		DOF 229, DOFPulse	
	ElseIf L11.state=0 And L12.state=1 And L15.state=1 And L16.state=1  Then '*AGE
		DOF 230, DOFPulse	
	ElseIf L11.state=0 And L12.state=0 And L15.state=1 And L16.state=1  Then '**GE
		DOF 231, DOFPulse
	End If
end Sub
sub SW5_hit() '**G* in (RAGE)
	If SuperLanes = True Then
		AddScoreChestJackpot = AddScoreChestJackpot + AddScoreSuperLanes
	End If
	If RetroMode = 1 or RetroMode = 3 Then
		playmedia "Sound-0x0113.mp3","Audionoise",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	Else
		'normal mode
'		addscore 1000
	End If
	If LastChanceStart = False Then
		addscore 1030
		BaseBonus = BaseBonus + 1800
		L15.state=1
		If L11.state=1 And L12.state=0 And L15.state=0 And L16.state=0  Then 'R***
			DOF 220, DOFPulse
		ElseIf L11.state=0 And L12.state=1 And L15.state=0 And L16.state=0  Then '*A**
			DOF 221, DOFPulse
		ElseIf L11.state=0 And L12.state=0 And L15.state=1 And L16.state=0  Then '**G*
			DOF 222, DOFPulse
		ElseIf L11.state=0 And L12.state=0 And L15.state=0 And L16.state=1  Then '***E
			DOF 223, DOFPulse
		ElseIf L11.state=1 And L12.state=1 And L15.state=0 And L16.state=0  Then 'RA**
			DOF 224, DOFPulse	
		ElseIf L11.state=1 And L12.state=0 And L15.state=1 And L16.state=0  Then 'R*G*
			DOF 225, DOFPulse
		ElseIf L11.state=1 And L12.state=0 And L15.state=0 And L16.state=1  Then 'R**E
			DOF 226, DOFPulse
		ElseIf L11.state=1 And L12.state=1 And L15.state=1 And L16.state=0  Then 'RAG*
			DOF 227, DOFPulse	
		ElseIf L11.state=1 And L12.state=0 And L15.state=1 And L16.state=1  Then 'R*GE
			DOF 228, DOFPulse	
		ElseIf L11.state=0 And L12.state=1 And L15.state=1 And L16.state=0  Then '*AG*
			DOF 229, DOFPulse	
		ElseIf L11.state=0 And L12.state=1 And L15.state=1 And L16.state=1  Then '*AGE
			DOF 230, DOFPulse	
		ElseIf L11.state=0 And L12.state=0 And L15.state=1 And L16.state=1  Then '**GE
			DOF 231, DOFPulse
		End If
	End If
end Sub
sub SW6_hit() '***E in (RAGE)
	If SuperLanes = True Then
		AddScoreChestJackpot = AddScoreChestJackpot + AddScoreSuperLanes
	End If
	If RetroMode = 1 or RetroMode = 3 Then
		playmedia "Sound-0x0113.mp3","Audionoise",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	Else
		'normal mode
'		addscore 1000
	End If
	addscore 1030
	BaseBonus = BaseBonus + 1800
	L16.state=1
	If L11.state=1 And L12.state=0 And L15.state=0 And L16.state=0  Then 'R***
		DOF 220, DOFPulse
	ElseIf L11.state=0 And L12.state=1 And L15.state=0 And L16.state=0  Then '*A**
		DOF 221, DOFPulse
	ElseIf L11.state=0 And L12.state=0 And L15.state=1 And L16.state=0  Then '**G*
		DOF 222, DOFPulse
	ElseIf L11.state=0 And L12.state=0 And L15.state=0 And L16.state=1  Then '***E
		DOF 223, DOFPulse
	ElseIf L11.state=1 And L12.state=1 And L15.state=0 And L16.state=0  Then 'RA**
		DOF 224, DOFPulse	
	ElseIf L11.state=1 And L12.state=0 And L15.state=1 And L16.state=0  Then 'R*G*
		DOF 225, DOFPulse
	ElseIf L11.state=1 And L12.state=0 And L15.state=0 And L16.state=1  Then 'R**E
		DOF 226, DOFPulse
	ElseIf L11.state=1 And L12.state=1 And L15.state=1 And L16.state=0  Then 'RAG*
		DOF 227, DOFPulse	
	ElseIf L11.state=1 And L12.state=0 And L15.state=1 And L16.state=1  Then 'R*GE
		DOF 228, DOFPulse	
	ElseIf L11.state=0 And L12.state=1 And L15.state=1 And L16.state=0  Then '*AG*
		DOF 229, DOFPulse	
	ElseIf L11.state=0 And L12.state=1 And L15.state=1 And L16.state=1  Then '*AGE
		DOF 230, DOFPulse	
	ElseIf L11.state=0 And L12.state=0 And L15.state=1 And L16.state=1  Then '**GE
		DOF 231, DOFPulse
	End If
end Sub

sub SW66_hit()  'sub SW66_hit(f) W** in (WAR)
	If SuperLanes = True Then
		AddScoreChestJackpot = AddScoreChestJackpot + AddScoreSuperLanes
	End If
	If RetroMode = 1 or RetroMode = 3 Then
		playmedia "Sound-0x0113.mp3","Audionoise",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	Else
		'normal mode
		'addscore 1000
	End If
	'f=f+1
	addscore 1030
	BaseBonus = BaseBonus + 1800
	L115.state=1
	'checktrigger
end Sub

sub SW67_hit() 'sub SW67_hit(g) *A* in (WAR)
	If SuperLanes = True Then
		AddScoreChestJackpot = AddScoreChestJackpot + AddScoreSuperLanes
	End If
	If RetroMode = 1 or RetroMode = 3 Then
		playmedia "Sound-0x0113.mp3","Audionoise",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	Else
		'normal mode
	'	addscore 1000
	End If
	addscore 1030
	BaseBonus = BaseBonus + 1800
	L116.state=1
	'checktrigger
end Sub

sub SW68_hit() 'sub SW68_hit(h) **R in (WAR)
	If SuperLanes = True Then
		AddScoreChestJackpot = AddScoreChestJackpot + AddScoreSuperLanes
	End If
	If RetroMode = 1 or RetroMode = 3 Then
		playmedia "Sound-0x0113.mp3","Audionoise",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	Else
		'normal mode
		'addscore 1000
	End If
	addscore 1030
	BaseBonus = BaseBonus + 1800
	L117.state=1
	'checktrigger
end Sub

sub SW81_hit()
	AddScore 30
	BaseBonus = BaseBonus + 1800
	Loops = Loops + 250000
	If TopBallLoop = 1 Then
		AddScoreLoopBonus = AddScoreLoopBonus + 250000
		AddScore AddScoreLoopBonus
		If MudBog(CurrentPlayer) = 2 And CurrentMissionFlag(CurrentPlayer) = 1 Then
			If OldMudBogDefeated <> MudBogDefeated(CurrentPlayer) Then
				AddScoreMudBogBonusLoopSliced = AddScoreMudBogBonusLoopSliced + 450000
				AddScoreMudBogBonusLoopSealed = AddScoreMudBogBonusLoopSealed + 900000
			Else
				AddScoreMudBogBonusLoopSliced = AddScoreMudBogBonusLoopSliced + 250000
				AddScoreMudBogBonusLoopSealed = AddScoreMudBogBonusLoopSealed + 650000
			End If
		End If
		If MoltenFire(CurrentPlayer) = 2 And CurrentMissionFlag(CurrentPlayer) = 1 Then
			If OldMoltenFireDefeated <> MoltenFireDefeated(CurrentPlayer) Then
				AddScoreMoltenFireBonusLoop = AddScoreMoltenFireBonusLoop + 900000
			Else
				AddScoreMoltenFireBonusLoop = AddScoreMoltenFireBonusLoop + 650000
			End If
		End If

		If BurningSands(CurrentPlayer) = 2 And CurrentMissionFlag(CurrentPlayer) = 1 Then
			If OldBurningSandsDefeated <> BurningSandsDefeated(CurrentPlayer) Then
				AddScoreSandWormLoop = AddScoreSandWormLoop + 900000
			Else
				AddScoreSandWormLoop = AddScoreSandWormLoop + 650000
			End If
		End If
	
		If WickedCavern(CurrentPlayer) = 2 And CurrentMissionFlag(CurrentPlayer) = 1 Then
			If OldWickedCavernDefeated <> WickedCavernDefeated(CurrentPlayer) Then
				AddScoreWickedCavernLoop = AddScoreWickedCavernLoop + 900000
			Else
				AddScoreWickedCavernLoop = AddScoreWickedCavernLoop + 650000
			End If
		End If

		If DeepFreeze(CurrentPlayer) = 2 And CurrentMissionFlag(CurrentPlayer) = 1 Then
			If OldDeepFreezeDefeated <> DeepFreezeDefeated(CurrentPlayer) Then
				AddScoreDeepFreezeFrozenBonusLoop = AddScoreDeepFreezeFrozenBonusLoop + 450000
				AddScoreDeepFreezeAwardedBonusLoop = AddScoreDeepFreezeAwardedBonusLoop + 900000
			Else
				AddScoreDeepFreezeFrozenBonusLoop = AddScoreDeepFreezeFrozenBonusLoop + 250000
				AddScoreDeepFreezeAwardedBonusLoop = AddScoreDeepFreezeAwardedBonusLoop + 650000
			End If
		End If
	
		if BlackCastle(CurrentPlayer) = 2 And CurrentMissionFlag(CurrentPlayer) = 1 Then
			If OldBlackCastleDefeated <> BlackCastleDefeated(CurrentPlayer) Then
				AddScoreBlackCastleLoop = AddScoreBlackCastleLoop + 900000
			Else
				AddScoreBlackCastleLoop = AddScoreBlackCastleLoop + 650000
			End If
		End If

		Flash_lamp.Enabled=True
		L142.State=2
		L143.State=2
		L270.State=0
		L271.State=0
		L272.State=0
		videoshotput
	End If
	OldMudBogDefeated = MudBogDefeated(CurrentPlayer)
	OldMoltenFireDefeated = MoltenFireDefeated(CurrentPlayer)
	OldBurningSandsDefeated = BurningSandsDefeated(CurrentPlayer)
	OldWickedCavernDefeated = WickedCavernDefeated(CurrentPlayer)
	OldDeepFreezeDefeated = DeepFreezeDefeated(CurrentPlayer)
	OldBlackCastleDefeated = BlackCastleDefeated(CurrentPlayer)
End Sub

Sub SW80_Hit()
	AddScore 30
	BaseBonus = BaseBonus + 1800
	TopBallLoop = 1
	sw80Timer.Enabled=True
End Sub

Sub sw80Timer_timer()
	sw80Timer.Enabled=False
	TopBallLoop = 0
End Sub


'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************

Sub RightSlingShot_Slingshot
	If SuperSlings = True Then
		AddScoreChestJackpot = AddScoreChestJackpot + AddScoreSuperSlings
	End If
	AddScore 10170
	BaseBonus = BaseBonus + 1800
    PlaySound SoundFXDOF("right_slingshot", 105, DOFPulse, DOFContactors)
	DOF 211, DOFPulse
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.rotx = 20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
'	gi001.State = 0:Gi002.State = 0
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.rotx = 10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.rotx = 0:RightSlingShot.TimerEnabled = 0':gi001.State = 1:Gi002.State = 1
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	If SuperSlings = True Then
		AddScoreChestJackpot = AddScoreChestJackpot + AddScoreSuperSlings
	End If
	AddScore 10170
	BaseBonus = BaseBonus + 1800
    PlaySound SoundFXDOF("left_slingshot", 103, DOFPulse, DOFContactors)
	DOF 210, DOFPulse
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.rotx = 20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1					
'	gi003.State = 0:Gi004.State = 0		'Use to restore light gi003 & gi004   original don't use this 
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.rotx = 0:LeftSlingShot.TimerEnabled = 0':gi003.State = 1:Gi004.State = 1
    End Select
    LStep = LStep + 1
End Sub



'********************************************
'**************	TARGETS**********************
'********************************************

sub SW56_hit()
	OrbitFlag = False
	OrbitStatus.Enabled = True
	
	If CurrentMissionFlag(CurrentPlayer) = 0 Then
		If CatapultModeFlag = False Then
			Select Case Int(Rnd*20)+1
				Case 1 : audioknight = "Sound-0x0178.mp3":SpeakTime = 3315	'Super Feats is Boosted
				Case 2 : audioknight = "Sound-0x017E.mp3":SpeakTime = 1553	'Super Feats is Boosted
				Case 3 : audioknight = "Sound-0x017F.mp3":SpeakTime = 1294 	'Super Feats is Boosted
				Case 4 : audioknight = "Sound-0x0182.mp3":SpeakTime = 5708	'Super Feats is Boosted
				Case 5 : audioknight = "Sound-0x0185.mp3":SpeakTime = 2791	'Super Feats is Boosted
				Case 6 : audioknight = "Sound-0x018A.mp3":SpeakTime = 2720 	'Super Feats is Boosted
				Case 7 : audioknight = "Sound-0x018B.mp3":SpeakTime = 1766	'Super Feats is Boosted
				Case 8 : audioknight = "Sound-0x0195.mp3":SpeakTime = 2615	'Super Feats is Boosted
				Case 9 : audioknight = "Sound-0x0197.mp3":SpeakTime = 2134 	'Super Feats is Boosted
				Case 10 : audioknight = "Sound-0x0198.mp3":SpeakTime = 1811	'Super Feats is Boosted
				Case 11 : audioknight = "Sound-0x019C.mp3":SpeakTime = 1032	'Super Feats is Boosted
				Case 12 : audioknight = "Sound-0x01A1.mp3":SpeakTime = 2812	'Super Feats is Boosted
				Case 13 : audioknight = "Sound-0x01A4.mp3":SpeakTime = 1217 'Super Feats is Boosted
				Case 14 : audioknight = "Sound-0x01A5.mp3":SpeakTime = 1484	'Super Feats is Boosted
				Case 15 : audioknight = "Sound-0x01BD.mp3":SpeakTime = 3705	'Super Feats is Boosted
				Case 16 : audioknight = "Sound-0x01BE.mp3":SpeakTime = 2414 'Super Feats is Boosted
				Case 17 : audioknight = "Sound-0x01BF.mp3":SpeakTime = 2288	'Super Feats is Boosted
				Case 18 : audioknight = "Sound-0x022B.mp3":SpeakTime = 2084	'Super Feats is Boosted
				Case 19 : audioknight = "Sound-0x026A.mp3":SpeakTime = 3331 'Super Feats is Boosted
				Case 20 : audioknight = "Sound-0x02C1.mp3":SpeakTime = 896	'Super Feats is Boosted
			End Select
			LightEyesBK
			playmedia audioknight,"audioknight",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		End If
	End If


	If SuperTargets = True Then
		AddScoreChestJackpot = AddScoreChestJackpot + AddScoreSuperTargets
	End If
	b=b+1
	playsound "target"
	addscore 1000:L89.state=1
	BaseBonus = BaseBonus + 1800
	if b=2 then L88.state=1 end If

	If CurrentMissionFlag(CurrentPlayer) = 0 And BlackKnightRetro(CurrentPlayer) <> 3 And CatapultModeFlag = False Then
		NumberOfHitForStartMission(CurrentPlayer) = NumberOfHitForStartMission(CurrentPlayer) + 1
		ActionWhenHitOneOfThreeWay
	End If
'	ActionWhenHitOneOfThreeWay
	If CurrentMissionFlag(CurrentPlayer) = 1  Or Knight_challenge_flag = True Or CatapultModeFlag = True or WarHurryFlag = True Then
		If L85State(CurrentPlayer) = 2 Then
			MissionHit 56
		End If
	End If
	CheckLamp.Enabled = True
	If SuperIsLit = True Then
		SuperModeRandom
		SuperFeatures = SuperFeatures + 75000

	End If
end sub

Sub SuperModeRandom()
	Select Case Int(Rnd*6)+1
		Case 1 : SelectSuperTargets = True
		Case 2 : SelectSuperSlings = True
		Case 3 : SelectSuperLanes = True
		Case 4 : SelectSuperPops = True 
		Case 5 : SelectSuperOrbits = True
		Case 6 : SelectSuperSpinner = True
	End Select
	SuperModeRandomCheck.Enabled = True
End Sub

Sub ChangeSuperModeColor
	If ChangeSuperMode = 0 Then 
		SuperFeaturesColor = "Yellow"
		ChangeSuperMode = 1
	Elseif ChangeSuperMode = 1 Then 
		SuperFeaturesColor = "Red"
		ChangeSuperMode = 2
	Elseif ChangeSuperMode = 2 Then 
		SuperFeaturesColor = "Magenta"
		ChangeSuperMode = 3
	Elseif ChangeSuperMode = 3 Then 
		SuperFeaturesColor = "Green"
		ChangeSuperMode = 0	
	End If
End Sub

Sub SuperModeRandomCheck_timer()
	SuperModeRandomCheck.Enabled = False
	If SelectSuperTargets = True Then 			'*************** Super Target ****************
		If SuperTargets = True Then
			SuperModeRandom
		Else
			playmedia "Sound-0x0867.mp3","Audiocomment",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			SuperTargets = True
			SelectSuperTargets = False
			TimeSuperMode.Enabled = True
			SuperModeDelay = 30
			'/* UpdateSuperOverlay
		'/*
		restoreOverlay.Interval=100
		restoreOverlay.Enabled=True
		'/*
			SuperIsLit = False
		End If
	End If

	If SelectSuperSlings = True Then 			'*************** Super Slings ****************
		If SuperSlings = True Then
			SuperModeRandom
		Else
			playmedia "Sound-0x0830.mp3","Audiocomment",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			SuperSlings = True
			SelectSuperSlings = False
			TimeSuperMode.Enabled = True
			SuperModeDelay = 30
			'/* UpdateSuperOverlay
		'/*
		restoreOverlay.Interval=100
		restoreOverlay.Enabled=True
		'/*
			SuperIsLit = False
		End If
	End If

	If SelectSuperLanes = True Then 			'*************** Super Lanes ****************
		If SuperLanes = True Then
			SuperModeRandom
		Else
			playmedia "Sound-0x0825.mp3","Audiocomment",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			SuperLanes = True
			SelectSuperLanes = False
			TimeSuperMode.Enabled = True
			SuperModeDelay = 30
			'/* UpdateSuperOverlay
		'/*
		restoreOverlay.Interval=100
		restoreOverlay.Enabled=True
		'/*
			SuperIsLit = False
		End If
	End If

	If SelectSuperPops = True Then 				'*************** Super Pops ****************
		If SuperPops = True Then
			SuperModeRandom
		Else
			playmedia "Sound-0x0880.mp3","Audiocomment",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			SuperPops = True
			SelectSuperPops = False
			TimeSuperMode.Enabled = True
			SuperModeDelay = 30
			'/* UpdateSuperOverlay
		'/*
		restoreOverlay.Interval=100
		restoreOverlay.Enabled=True
		'/*
			SuperIsLit = False
		End If
	End If

	If SelectSuperOrbits = True Then 			'*************** Super Orbits ****************
		If SuperOrbits = True Then
			SuperModeRandom
		Else
			playmedia "Sound-0x0856.mp3","Audiocomment",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			SuperOrbits = True
			SelectSuperOrbits = False
			TimeSuperMode.Enabled = True
			SuperModeDelay = 30
			'/* UpdateSuperOverlay
		'/*
		restoreOverlay.Interval=100
		restoreOverlay.Enabled=True
		'/*
			SuperIsLit = False
		End If
	End If

	If SelectSuperSpinner = True Then 			'*************** Super Spinner ****************
		If SuperSpinner = True Then
			SuperModeRandom
		Else
			playmedia "Sound-0x0827.mp3","Audiocomment",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			SuperSpinner = True
			SelectSuperSpinner = False
			TimeSuperMode.Enabled = True
			SuperModeDelay = 30
			'/* UpdateSuperOverlay
		'/*
		restoreOverlay.Interval=100
		restoreOverlay.Enabled=True
		'/*
			SuperIsLit = False
		End If
	End If

End Sub

Sub StopSuperMode
	PuPlayer.LabelSet pBackglass,"AddScoreChest","",1,"{'mt':2,'color':28671, 'size': 5, 'xpos': 14, 'xalign': 1, 'ypos': 69, 'yalign': 1}"
		PuPlayer.LabelSet pBackglass,"TimerSuperMode","",1,"{'mt':2,'color':16777215, 'size': 6, 'xpos': 5, 'xalign': 1, 'ypos': 9, 'yalign': 1}"
		SuperTargets = False
		SuperSlings = False
		SuperLanes = False
		SuperLanes = False
		SuperPops = False
		SuperOrbits = False
		SuperSpinner = False
		L29State = 0
		L35State = 0
		L41State = 0
		L47State = 0
		L53State = 0
		L59State = 0
		CheckLamp.Enabled = True
		TimeSuperMode.Enabled = False
		'/* UpdateSuperOverlay
		'/*
		restoreOverlay.Interval=100
		restoreOverlay.Enabled=True
		'/*
		AddScoreChestJackpot = 0
		AddChestJackpot = False
End Sub

'	PuPlayer.LabelNew pBackglass,"AddScoreChestJackpot",numberfont,		6,28671	,0,0,1,14,69,1,1
'	PuPlayer.LabelNew pBackglass,"TimerSuperMode",numberfont,			6,16777215	,0,0,1,5,9,1,1

Sub TimeSuperMode_Timer()
	SuperModeDelay = SuperModeDelay - 1
	PuPlayer.LabelSet pBackglass,"TimerSuperMode",SuperModeDelay,1,"{'mt':2,'color':16777215, 'size': 6, 'xpos': 5, 'xalign': 1, 'ypos': 9, 'yalign': 1}"
	If SuperModeDelay <= 2 And AddChestJackpot = True Then
		PuPlayer.LabelSet pBackglass,"AddScoreChest",""&FormatNumber(AddScoreChestJackpot,0),1,"{'mt':2,'color':28671, 'size': 5, 'xpos': 14, 'xalign': 1, 'ypos': 69, 'yalign': 1}"
		PuPlayer.LabelSet pBackglass,"TimerSuperMode","",1,"{'mt':2,'color':16777215, 'size': 6, 'xpos': 5, 'xalign': 1, 'ypos': 9, 'yalign': 1}"
	End If
	If SuperModeDelay <= 0 Then
		StopSuperMode
	End If
End Sub

sub SW59_hit()
	playsound "target"
	OrbitFlag = False
	OrbitStatus.Enabled = True
	If SuperTargets = True Then
		AddScoreChestJackpot = AddScoreChestJackpot + AddScoreSuperTargets
	End If
	addscore 1000
	BaseBonus = BaseBonus + 1800
	L101.state=1
	If Knight_challenge_flag = False Then
		If Knight_challenge(CurrentPlayer) = 0 Then
			Video_lock1IsLit
			Knight_challenge(CurrentPlayer) = 1
		ElseIf Knight_challenge(CurrentPlayer) = 2 Then
			Video_lock2IsLit
			Knight_challenge(CurrentPlayer) = 3
		ElseIf Knight_challenge(CurrentPlayer) = 4 Then
			Video_lock3IsLit
			Knight_challenge(CurrentPlayer) = 5
		Else	
		End If
	End If
	If CurrentMissionFlag(CurrentPlayer) = 1  Or Knight_challenge_flag = True Or CatapultModeFlag = True or WarHurryFlag = True Then
		If L98State(CurrentPlayer) = 2 Then
			MissionHit 59
		End If
	End If
	CheckLamp.Enabled=True
end sub

sub SW26_hit()
	If SuperTargets = True Then
		AddScoreChestJackpot = AddScoreChestJackpot + AddScoreSuperTargets
	End If
	If RetroMode = 1 or RetroMode = 3 Then
		playmedia "Sound-0x0160.mp3","Audionoise",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	Else
'	normal mode
'		addscore 1000
	End If
	mb1=1
	addscore 10030
	BaseBonus = BaseBonus + 1800
	playsound "target"
	L63.state=1
	'if mb1+mb2+mb3+mb4+mb5=5 then addmultiball 3 end If
	'addmultiball 2
	If CurrentMissionFlag(CurrentPlayer) = 1 Then
		AddTimeForMission(CurrentPlayer) = AddTimeForMission(CurrentPlayer) + 4
	End If
end sub

sub SW27_hit()
	If SuperTargets = True Then
		AddScoreChestJackpot = AddScoreChestJackpot + AddScoreSuperTargets
	End If
	If RetroMode = 1 or RetroMode = 3 Then
		playmedia "Sound-0x0160.mp3","Audionoise",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		
	Else
		'normal mode
		'	addscore 1000
	End If
	mb2=1
	addscore 10030
	BaseBonus = BaseBonus + 1800
	playsound "target"
	L64.state=1
	If CurrentMissionFlag(CurrentPlayer) = 1 Then
		AddTimeForMission(CurrentPlayer) = AddTimeForMission(CurrentPlayer) + 4
	End If
end sub

sub SW33_hit()
	If SuperTargets = True Then
		AddScoreChestJackpot = AddScoreChestJackpot + AddScoreSuperTargets
	End If
If RetroMode = 1 or RetroMode = 3 Then
	playmedia "Sound-0x0160.mp3","Audionoise",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
Else
	'normal mode
'	addscore 1000
End If
	mb3=1
	addscore 10030
	BaseBonus = BaseBonus + 1800
	playsound "target"
	L110.state=1
	L110Timer.Enabled=True
end sub

sub SW34_hit()
	If SuperTargets = True Then
		AddScoreChestJackpot = AddScoreChestJackpot + AddScoreSuperTargets
	End If
If RetroMode = 1 or RetroMode = 3 Then
	playmedia "Sound-0x0160.mp3","Audionoise",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
Else
	'normal mode
'	addscore 1000
End If
	mb4=1
	addscore 10030
	BaseBonus = BaseBonus + 1800
	playsound "target"
	L111.state=1
	L111Timer.Enabled=True
end sub

sub SW35_hit()
	If SuperTargets = True Then
		AddScoreChestJackpot = AddScoreChestJackpot + AddScoreSuperTargets
	End If
If RetroMode = 1 or RetroMode = 3 Then
	playmedia "Sound-0x0160.mp3","Audionoise",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
Else
	'normal mode
'	addscore 1000
End If
	mb5=1
	addscore 10030
	BaseBonus = BaseBonus + 1800
	playsound "target"
	L112.state=1
	L112Timer.Enabled=True
end sub

Sub L110Timer_timer()
	L110.state=2
	L110Timer.Enabled = False
End Sub

Sub L111Timer_timer()
	L111.state=2
	L111Timer.Enabled = False
End Sub

Sub L112Timer_timer()
	L112.state=2
	L112Timer.Enabled = False
End Sub

'********************************************
'***********MAGNET***************************
'********************************************

Set mMagnetSave = New cvpmMagnet : With mMagnetSave
	.InitMagnet Magnet, 15
End With

Sub Magnet_Hit():mMagnetSave.AddBall ActiveBall: End Sub
Sub Magnet_UnHit(): mMagnetSave.RemoveBall ActiveBall: End Sub



sub SW100_hit()
'	mMagnetSave.MagnetOn = 1
'	magnettimer.enabled=1
'	L24.State=1
end sub

sub magnettimer_timer()
	magnettimer.enabled=0
	mMagnetSave.MagnetOn = 0
	L24.State=0
	MagnaSaveFlag(CurrentPlayer) = 0
	DOF 201, DOFOff
end Sub

'********************************************
'***********DROP TARGETS*********************
'********************************************

Sub SW58_hit()

	If CurrentMissionFlag(CurrentPlayer) = 0 Then
		Select Case Int(Rnd*20)+1
			Case 1 : audioknight = "Sound-0x0199.mp3":SpeakTime = 1347 'Super Feats is Boosted
			Case 2 : audioknight = "Sound-0x01C1.mp3":SpeakTime = 1464 'Super Feats is Boosted
			Case 3 : audioknight = "Sound-0x01D4.mp3":SpeakTime = 1393 	'Super Feats is Boosted
			Case 4 : audioknight = "Sound-0x01E4.mp3":SpeakTime = 1380	'Super Feats is Boosted
			Case 5 : audioknight = "Sound-0x0207.mp3":SpeakTime = 1361	'Super Feats is Boosted
			Case 6 : audioknight = "Sound-0x0247.mp3":SpeakTime = 1149 	'Super Feats is Boosted
			Case 7 : audioknight = "Sound-0x0269.mp3":SpeakTime = 1306	'Super Feats is Boosted
			Case 8 : audioknight = "Sound-0x026E.mp3":SpeakTime = 1256	'Super Feats is Boosted
			Case 9 : audioknight = "Sound-0x039A.mp3":SpeakTime = 1246 	'Super Feats is Boosted
			Case 10 : audioknight = "Sound-0x03C5.mp3":SpeakTime = 1237	'Super Feats is Boosted
			Case 11 : audioknight = "Sound-0x018D.mp3":SpeakTime = 2684	'Super Feats is Boosted
			Case 12 : audioknight = "Sound-0x01D0.mp3":SpeakTime = 1914	'Super Feats is Boosted
			Case 13 : audioknight = "Sound-0x026B.mp3":SpeakTime = 1907	'Super Feats is Boosted
			Case 14 : audioknight = "Sound-0x027F.mp3":SpeakTime = 2252	'Super Feats is Boosted
			Case 15 : audioknight = "Sound-0x0307.mp3":SpeakTime = 2433	'Super Feats is Boosted
			Case 16 : audioknight = "Sound-0x0326.mp3":SpeakTime = 2704	'Super Feats is Boosted
			Case 17 : audioknight = "Sound-0x0337.mp3":SpeakTime = 2161	'Super Feats is Boosted
			Case 18 : audioknight = "Sound-0x0427.mp3":SpeakTime = 2303	'Super Feats is Boosted
			Case 19 : audioknight = "Sound-0x045D.mp3":SpeakTime = 2074	'Super Feats is Boosted
			Case 20 : audioknight = "Sound-0x0485.mp3":SpeakTime = 2009	'Super Feats is Boosted
		End Select
		LightEyesBK
		playmedia audioknight,"audioknight",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	End If

	OrbitFlag = False
	OrbitStatus.Enabled = True
'	playSound "fx_shieldhit"
If ShieldRemainsLock = True Then
	Shield_New_Status = "unlock"
'	ShieldTimer.Enabled=True
	ShieldNumberOfMove = 2
	ShieldTimer.interval=100
	ShieldTimer.Enabled=True
	
Else
	Shield_New_Status = "Unlock"
	ShieldTimer.Enabled=True
End If
	If CurrentMissionFlag(CurrentPlayer) = 0 And BlackKnightRetro(CurrentPlayer) <> 3 And CatapultModeFlag = False Then
		NumberOfHitForStartMission(CurrentPlayer) = NumberOfHitForStartMission(CurrentPlayer) + 1
		ActionWhenHitOneOfThreeWay
	End If
	addscore 1000
	BaseBonus = BaseBonus + 1800
	SW58.rotx=90
'	Videos_Ball_Saved
	If RetroMode = 1 or RetroMode = 3 Then
		playmedia "Sound-0x0160.mp3","Audionoise",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	Else
		'normal mode
'		addscore 1000
	End If
		addscore 1000
		BaseBonus = BaseBonus + 1800
	If CurrentMissionFlag(CurrentPlayer) = 1  Or Knight_challenge_flag = True Or CatapultModeFlag = True or WarHurryFlag = True Then
		If L91State(CurrentPlayer) = 2 Then
			MissionHit 58
		End If
	End If
	CheckLamp.Enabled = True
end Sub

Sub SW82_hit()
	If SuperTargets = True Then
		AddScoreChestJackpot = AddScoreChestJackpot + AddScoreSuperTargets
	End If
'	AddScoreSW82 = AddScoreSW82 + 250000
'	AddScore AddScoreSW82
	If RetroMode = 1 or RetroMode = 3 Then
		playmedia "Sound-0x0047.mp3","Audionoise",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	Elseif CatapultModeFlag = True Then
		If L131.state=0 Then 
			L131.State = 2 
			Catapult_Super_Jackpot_Multi = 2			
		Elseif L132.state=0 Then 
			L132.State = 2
			Catapult_Super_Jackpot_Multi = 3
		Else
			L133.state=2
			Catapult_Super_Jackpot_Multi = 4
		End If
	Else
		SaveStateLightturnlight
		SaveStateLight
		animation_start_and_finish_flag = 1
		TurnOffAllRegularLights
		seq_animation_array = Array (2,3,18)
		SeqAnimation = 0
		LightsequenceAnimation.Enabled = True
		If catapult_lock_is_lit(CurrentPlayer) = True Then
			If CurrentMissionFlag(CurrentPlayer) = 1  Or Knight_challenge_flag = True Or CatapultModeFlag = True or WarHurryFlag = True Then
				MissionHit 82
			End If
		End If
		If catapult_lock_is_lit(CurrentPlayer) = False And CatapultModeFlag = False Then
			video_catapult_lock_is_lit 
			'PlaySound "Sound-0x08E1"
			playmedia "Sound-0x08E1.mp3","Audiocomment",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			If BallInCatapult(CurrentPlayer) = 0 Then
				L131.state=2
				L132.state=0
				L133.state=0
			End If
		Elseif catapult_lock_is_lit(CurrentPlayer) = True And CatapultModeFlag = False Then
			playmedia "Sound-0x0845.mp3","Audiocomment",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		End If
		If CatapultModeFlag = False Then
			catapult_lock_is_lit(CurrentPlayer) = True
		End If
	End If 
AddScore 10030
BaseBonus = BaseBonus + 1800
playsound "target"
End Sub

'********************************************
'**********KICKER****************************
'********************************************
sub SW22_hit
	GiOn
	BallControlTimer.Enabled = False
End Sub

sub SW23_hit
	PlaySoundAt "fx_launchball", Plunger
	DOF 206, DOFPulse
End Sub

sub SW60_hit
	PlaySound ""
	OrbitFlag = False
	OrbitStatus.Enabled = True
	SW60videoDelay = 100
	SaveStateLight
	If ExtraBallsIsLit = True Then
		ExtraBallsIsLit = False
		SW60videoDelay = 9000
		VideoExtraBall
		AwardExtraBall
	End If
	If CurrentMissionFlag(CurrentPlayer) = 1  Or Knight_challenge_flag = True Or CatapultModeFlag = True or WarHurryFlag = True Then
		If L91State(CurrentPlayer) = 2 Then
			MissionHit 58
			SW60videoDelay =  SW60videoDelay + 4100
		End If
	End If
'	SW60video.Interval = SW60videoDelay
'	SW60video.Enabled = True

	SW60Mystery.Interval = SW60videoDelay
	SW60Mystery.Enabled = True

end sub

Sub SW60Mystery_timer()
	SW60Mystery.Enabled = False
	SW60videoDelay = 100
	If MysteryFlag = True Then
		Video_Mistery
		SW60videoDelay = 6000
		SequenceAnimation = 1
		TableAnimation
	end If
	SW60video.Interval = SW60videoDelay
	SW60video.Enabled = True
End Sub

Sub SW60video_Timer()
	SW60video.Enabled = False
	If Shield_lock = False Then						'****************** Check if OK
		Shield_New_Status = "lock"
'		ShieldTimer.Enabled=True
		ShieldNumberOfMove = 1
		ShieldTimer.interval=250
		ShieldTimer.Enabled=True
	End If 
	If RetroMode = 0 Then
		MissionRandomTimer.Enabled=False
		If BouleAngle = 0 Then						'In test
			'StopBouleFlag = True					'In test
			StopBouleAngle = 90						'In test
			StopRotating.interval=100				'In test
			StopRotating.Enabled=True				'In test
		End If										'In test
		
		If Knight_challenge(CurrentPlayer) = 1 Then
			Knight_challenge(CurrentPlayer) = 2
			Video_knightball1lock
			'playsound "ball one locked Sound-0x0580"
			playmedia "ball one locked Sound-0x0580.mp3","Audiocomment",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			KnightChallengeTimer.Interval=7000
			KnightChallengeTimer.Enabled = True
		ElseIf Knight_challenge(CurrentPlayer) = 3 Then
			Knight_challenge(CurrentPlayer) = 4
			Video_knightball2lock
			'playsound "ball two locked Sound-0x01BE"
			playmedia "ball two locked Sound-0x01BE.mp3","Audiocomment",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			KnightChallengeTimer.Interval=7000
			KnightChallengeTimer.Enabled = True
		ElseIf Knight_challenge(CurrentPlayer) = 5 Then
			Knight_challenge(CurrentPlayer) = 6
			Video_knightball3lock
			'playsound "ball three locked"
			playmedia "ball three locked.mp3","Audiocomment",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			KnightChallengeTimer.Interval=7000
			KnightChallengeTimer.Enabled = True
		Else
'			Video_Mistery
			'playsound "ball three locked"
'			playmedia "ball three locked.mp3","Audiocomment",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			KnightChallengeTimer.Interval=100
			KnightChallengeTimer.Enabled = True
'			SequenceAnimation = 1
'			TableAnimation
			'timer004.Enabled = True
		end If
	Else
		timer004.Interval=100
		timer004.Enabled = True
	End If

End Sub

'*************************************************************************************************************
'******************************* Animated sequence + video **************************************************
'*************************************************************************************************************
			'seq_animation_array = Array (8,1,10,1,13,1,13,1,13)'hydra
			'seq_animation_array = Array (6,0,11,0,13,3,13)'MoltenFire
			'seq_animation_array = Array (6,3,10,3,13,3,13)'burning sand
			'seq_animation_array = Array (12,5,13,4,13,5,12,0,14,0,13,4,13)'WickedCavern
			'seq_animation_array = Array (6,5,15,5,13,2,13) 'Deep Freeze
			'seq_animation_array = Array (10,5,16,4,13,3,16,0,17,0,9) 'Black Castle
'*************************************************************************************************************
'*************************************************************************************************************
'*************************************************************************************************************
Sub KnightChallengeTimer_timer()
	SW60Timer=500
	SeqAnimation = 0
	SaveStateLightturnlight
	animation_start_and_finish_flag = 1
	If Knight_challenge(CurrentPlayer) = 6 Then
		'AddTimeForMission = MissionTime
		'horloge.Enabled=True
		'CurrentMissionFlag(CurrentPlayer) = 1
		'BeginCurrentMissionFlag = 1
'**********		AddMultiball 2
		Knight_challenge(CurrentPlayer) = 0
		kickball
		bMultiBallMode = True
		doublekick.Enabled=True
		KnightRemaining = 3
		Knight_challenge_flag = True
		Knight_challenge_phase = 0
		EnableBallSaver 30
		MusicCheck 60
		If CurrentMissionFlag(CurrentPlayer) = 0 Then
'			video_knight_challenge_background 3
			VideoSetBackground
		Else
			Knight_challenge_flag = True
		End If
	End If

	
If ShieldIsReadyToActivateMission(CurrentPlayer) = True And CatapultModeFlag = False And Knight_challenge_flag = False Then
	If WarHurryFlag = False Then MusicCheck 60 End If
	StartMission = 1
	If CurrentMissionFlag(CurrentPlayer) = 0 And WarHurryFlag = False Then
		If Timer2.Enabled=True Then
			StopBouleFlag = True					'In test
			StopBouleAngle = 90						'In test
			StopRotating.interval=100				'In test
			StopRotating.Enabled=True				'In test
		End If
		If MudBog(CurrentPlayer) = 1 And MissionRandom = "MudBog" Then
			MudBog(CurrentPlayer) = 2
			VideoHydraIntro
			'MissionRandomTimer.Enabled=False
			SW60Timer=8500
			TurnOffAllRegularLights
			seq_animation_array = Array (8,1,10,1,13,1,13,1,13)
			SeqAnimation = 0
			LightsequenceAnimation.Enabled = True
		End If
		If MoltenFire(CurrentPlayer) = 1 And MissionRandom = "MoltenFire" Then
			MoltenFire(CurrentPlayer) = 2
			VideoFireElementIntro
			'MissionRandomTimer.Enabled=False
			SW60Timer=6900
			TurnOffAllRegularLights
			seq_animation_array = Array (6,0,11,0,13,3,13)
			'seq_animation_array = Array (4,1,8,1,7)
			SeqAnimation = 0
			LightsequenceAnimation.Enabled = True
		End If
		If BurningSands(CurrentPlayer) = 1 And MissionRandom = "BurningSands" Then
			BurningSands(CurrentPlayer) = 2
			VideoSandwormIntro
			'MissionRandomTimer.Enabled=False
			SW60Timer=6000
			TurnOffAllRegularLights
			seq_animation_array = Array (6,3,10,3,13,3,13)
			SeqAnimation = 0
			LightsequenceAnimation.Enabled = True
		End If
		If WickedCavern(CurrentPlayer) = 1 And MissionRandom = "WickedCavern" Then
			WickedCavern(CurrentPlayer) = 2
			VideoHandHolderIntro
			'MissionRandomTimer.Enabled=False
			SW60Timer=8500
			TurnOffAllRegularLights
			SeqAnimation = 0
			seq_animation_array = Array (12,5,13,4,13,5,12,0,14,0,13,4,13)
			LightsequenceAnimation.Enabled = True
		End If
		If DeepFreeze(CurrentPlayer) = 1 And MissionRandom = "DeepFreeze" Then
			DeepFreeze(CurrentPlayer) = 2
			VideoLichIntro
			'MissionRandomTimer.Enabled=False
			SW60Timer=8500
			TurnOffAllRegularLights
			SeqAnimation = 0
			seq_animation_array = Array (6,5,15,5,13,2,13)
			LightsequenceAnimation.Enabled = True
		End If
		If BlackCastle(CurrentPlayer) = 1 And MissionRandom = "BlackCastle" Then	
			BlackCastle(CurrentPlayer) = 2
			VideoBlackCastleIntro
			SW60Timer=15500
			TurnOffAllRegularLights
			SeqAnimation = 0
			seq_animation_array = Array (10,5,16,4,13,3,16,0,17,0,9) 'Black Castle
			LightsequenceAnimation.Enabled = True
		End If
	End If
End If	
	timer004.Interval=SW60Timer
	timer004.Enabled = True
	KnightChallengeTimer.Enabled = False
	StartMission = 0
End Sub

sub timer004_Timer()
	timer004.Enabled = False
	playsound SoundFXDOF("ballrelease", 113, DOFPulse, DOFContactors)
	SW60.kick 300, 30, 1.56
	If RetroMode = 0 Then
		If CurrentMissionFlag(CurrentPlayer) = 0 Then
			If MudBog(CurrentPlayer) = 2 Then
				VideoHydraLoop
				'CurrentMissionTimer.Enabled=true
				AddTimeForMission(CurrentPlayer) = MissionTime
				'horloge.Enabled=True
				CurrentMissionFlag(CurrentPlayer) = 1
			End If
			If MoltenFire(CurrentPlayer) = 2 Then
				VideoFireElementLoop
				'CurrentMissionTimer.Enabled=true
				AddTimeForMission(CurrentPlayer) = MissionTime
				horloge.Enabled=True
				CurrentMissionFlag(CurrentPlayer) = 1
			End If
			If BurningSands(CurrentPlayer) = 2 Then
				VideoSandwormLoop
				'CurrentMissionTimer.Enabled=true
				AddTimeForMission(CurrentPlayer) = MissionTime
				horloge.Enabled=True
				CurrentMissionFlag(CurrentPlayer) = 1
				BurningSandsLightMovementNumber = 0
				BurningSandsLightMove.Enabled=True
			End If
			If WickedCavern(CurrentPlayer) = 2 Then
				VideoHandHolderLoop
				'CurrentMissionTimer.Enabled=true
				AddTimeForMission(CurrentPlayer) = MissionTime
				horloge.Enabled=True
				CurrentMissionFlag(CurrentPlayer) = 1
			End If
			If DeepFreeze(CurrentPlayer) = 2 Then
				VideoLichLoop
				'CurrentMissionTimer.Enabled=true
				AddTimeForMission(CurrentPlayer) = MissionTime
				horloge.Enabled=True
				CurrentMissionFlag(CurrentPlayer) = 1
			End If
			If BlackCastle(CurrentPlayer) = 2 Then
				VideoBlackCastleLoop
				'CurrentMissionTimer.Enabled=true
				AddTimeForMission(CurrentPlayer) = MissionTime
				horloge.Enabled=True
				CurrentMissionFlag(CurrentPlayer) = 1
			End If
		End If
	End If
	AddScore 10000
	BaseBonus = BaseBonus + 1800
	CheckLamp.Interval = 250
	CheckLamp.Enabled=True
end Sub

Sub CheckCurrentMissionPlayer
	If CurrentMissionFlag(CurrentPlayer) = 1 Then
		If MudBog(CurrentPlayer) = 2 Then
			horloge.Enabled=True
		End If
		If MoltenFire(CurrentPlayer) = 2 Then
			horloge.Enabled=True
		End If
		If BurningSands(CurrentPlayer) = 2 Then
			horloge.Enabled=True
			BurningSandsLightMovementNumber = 0
			BurningSandsLightMove.Enabled=True
		End If
		If WickedCavern(CurrentPlayer) = 2 Then
			horloge.Enabled=True
		End If
		If DeepFreeze(CurrentPlayer) = 2 Then
			horloge.Enabled=True
		End If
		If BlackCastle(CurrentPlayer) = 2 Then
			horloge.Enabled=True
		End If
	Else
		'none (when no mission is Start)
	End If
End Sub

Sub horloge_Timer()

 	If CurrentMissionFlag(CurrentPlayer) = 1 Then
'		If MudBog(CurrentPlayer) = 2 Then textmission="MudBog" : textdefeated=MudBogDefeated(CurrentPlayer)
'		If MoltenFire(CurrentPlayer) = 2 Then textmission="MoltenFire" : textdefeated=MoltenFireDefeated(CurrentPlayer)
'		If BurningSands(CurrentPlayer) = 2 Then textmission="BurningSands" : textdefeated=BurningSandsDefeated(CurrentPlayer)
'		If WickedCavern(CurrentPlayer) = 2 Then textmission="WickedCavern" : textdefeated=WickedCavernDefeated(CurrentPlayer)
'		If DeepFreeze(CurrentPlayer) = 2 Then textmission="DeepFreeze" : textdefeated=DeepFreezeDefeated(CurrentPlayer)
'		If BlackCastle(CurrentPlayer) = 2 Then textmission="BlackCastle" : textdefeated=BlackCastleDefeated(CurrentPlayer)
		AddTimeForMission(CurrentPlayer) = AddTimeForMission(CurrentPlayer) - 1
		If AddTimeForMission(CurrentPlayer) < 0 Then
			CurrentMissionFlag(CurrentPlayer) = 0
			BurningSandsLightMove.Enabled=False
'			If Knight_challenge_flag = True Then
'				video_knight_challenge_background KnightRemaining
'			End If
		End If
	End If
	If CurrentMissionFlag(CurrentPlayer) = 0 Then
		'CurrentMissionTimer.interval = 4000
		CurrentMissionTimer.Enabled=True
	End If
	If CurrentMissionFlag(CurrentPlayer) = 2 Then
		If BlackKnightRetro(CurrentPlayer) = 3 Then 					'Just to escape the next condition  (1 & 0)
			AddTimeForMission(CurrentPlayer) = AddTimeForMission(CurrentPlayer) - 1
			If AddTimeForMission(CurrentPlayer) < 0 Then
				StopPlayingGame		' = 1
				CurrentMissionFlag(CurrentPlayer) = 0
				Horloge.Enabled= False
				StopRansom = True
			End If
		End If
	End If
	If CurrentMissionFlag(CurrentPlayer) = 3 Then
		LastChanceDrainUncompleted = True
		LastChanceBallToLock = 3 - BallInCatapult(CurrentPlayer)
		LastChanceChronoStart(CurrentPlayer) = True
		If HideOverlay = True Then
			puPlayer.LabelSet pBackglass,"LastChanceBallToLock","",1,""
			puPlayer.LabelSet pBackglass,"LastChanceTime","",1,""
			puPlayer.LabelSet pBackglass,"LastChanceJackpot","",1,""
		Else
		puPlayer.LabelSet pBackglass,"LastChanceBallToLock","Lock "& FormatNumber(LastChanceBallToLock,0)&" Balls",1,""
		puPlayer.LabelSet pBackglass,"LastChanceTime",""& FormatNumber(AddTimeForMission(CurrentPlayer),0)&" ",1,""
		puPlayer.LabelSet pBackglass,"LastChanceJackpot","Jackpots : "& FormatNumber(AddscoreCatapult,0)&" ",1,""
		End If
		If LastChanceFlag = True Then
			AddTimeForMission(CurrentPlayer) = AddTimeForMission(CurrentPlayer) - 1
			If LastChanceSuccess = True Then
				playmedia "Sound-0x0026.mp3","Audionoise",pCallouts,"",1000,"",1,1 '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority) 
			End If  
			If AddTimeForMission(CurrentPlayer) <= 0 Then
				Flash_lamp.Enabled=True
				If LastChanceSuccess = False Then
'					StopPlayingGame
					DisableTable True
					bGameInPlay = False
'					MusicCheck 0
'					StopRansom = False
'					EndOfBall2
					PuPlayer.playstop pCallouts
					PuPlayer.playstop pCallouts
					playclear pMusic
					playclear pAudio
				Else
					LastChanceClearTable = True
					DisableTable True
					bGameInPlay = False
'					tilttime = 0
'					tilttableclear.enabled = true
'					TiltRecoveryTimer.Enabled = True 'start the Tilt delay to check for all the balls to be drained

'					StopPlayingGame
'					DisableTable True					'In test
					VideoSupercatapultBall3lock
				End If
				CurrentMissionFlag(CurrentPlayer) = 0
				horloge.Enabled = False
				LastChanceStart = False

			End If
		End If
	End If
End Sub

Sub CurrentMissionTimer_Timer()				'When The monster is not defeated
	CurrentMissionTimer.Enabled=False
	horloge.Enabled=False
	CurrentMissionFlag(CurrentPlayer) = 0
	If MudBog(CurrentPlayer) = 2 Then
		MudBog(CurrentPlayer) = 3
		LightObjectColor(1).State=0
	End If
	If MoltenFire(CurrentPlayer) = 2 Then
		MoltenFire(CurrentPlayer) = 3
		LightObjectColor(3).State=0
	End If
	If BurningSands(CurrentPlayer) = 2 Then
		BurningSands(CurrentPlayer) = 3
		LightObjectColor(5).State=0
	End If
	If WickedCavern(CurrentPlayer) = 2 Then
		WickedCavern(CurrentPlayer) = 3
		LightObjectColor(7).State=0
	End If
	If DeepFreeze(CurrentPlayer) = 2 Then
		DeepFreeze(CurrentPlayer) = 3
		LightObjectColor(9).State=0
	End If
	If BlackCastle(CurrentPlayer) = 2 Then
		BlackCastle(CurrentPlayer) = 3
		LightObjectColor(11).State=0
	End If
	If BlackCastle(CurrentPlayer) = 4 Then
		BlackCastle(CurrentPlayer) = 0
		MudBog(CurrentPlayer) = 0
		MoltenFire(CurrentPlayer) = 0
		BurningSands(CurrentPlayer) = 0
		WickedCavern(CurrentPlayer) = 0
		DeepFreeze(CurrentPlayer) = 0
		LightObjectColor(11).State=0
		TurnOffRoundLights
	End If
	MissionRandomTimer.Interval=500
	MissionRandomTimer.Enabled=True
	'SelectMission
	VideoSetBackground
End Sub

Sub AddToSkill(swNo):bsSkill.AddBall 0:End Sub

sub kicker006_hit()
'a=a+1
'if a=2 then addscore 2000 end if
'if a=3 then addscore 3000 end if
'if a=4 then addscore 4000 end if
'if a=5 then addscore 5000 end if 'multiball +1 en prevision
'if a=6 then addscore 6000 end if
'if a=7 then addscore 7000 end if
'if a=8 then addscore 8000 end if
'if a=9 then addscore 9000 end if
'if a=>10 Then  AddScore 10000 end if 'multiball +2 en prevision
timer002.Enabled = True
'addscore 1000
end sub

sub SW45_hit()
	OrbitFlag = False
	OrbitStatus.Enabled = True
	If AddScoreChestJackpot <> 0 Then
		addscore AddScoreChestJackpot
		ScoreSuperChampion(CurrentPlayer) = ScoreSuperChampion(CurrentPlayer) + AddScoreChestJackpot

		playmedia "Sound-0x0925.mp3","Audiocomment",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'		PuPlayer.LabelSet pBackglass,"AddScoreChest","score"&FormatNumber(AddScoreChestJackpot,0),1,"{'mt':2,'color':28671, 'size': 8, 'xpos': 14, 'xalign': 1, 'ypos': 69, 'yalign': 1}"
'		PuPlayer.LabelSet pBackglass,"TimerSuperMode","",1,"{'mt':2,'color':16777215, 'size': 6, 'xpos': 5, 'xalign': 1, 'ypos': 9, 'yalign': 1}"
		If PlayersPlayingGame = 1  Then
			playmedia "P1-Chest.png","PuPOverlays",pBackGlass,"cineon",100,"",1,7  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		Elseif PlayersPlayingGame = 2 Then
			playmedia "P2-Chest.png","PuPOverlays",pBackGlass,"cineon",100,"",1,7  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		Elseif PlayersPlayingGame = 3 Then
			playmedia "P3-Chest.png","PuPOverlays",pBackGlass,"cineon",100,"",1,7  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		Elseif PlayersPlayingGame = 4 Then
			playmedia "P4-Chest.png","PuPOverlays",pBackGlass,"cineon",100,"",1,7  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
		End If
		SuperModeDelay = 4
		AddChestJackpot = True
	End If
	SW60Timer=500
	AddScore 5000
	BaseBonus = BaseBonus + 1800
	TurnOffAllRegularLights
			seq_animation_array = Array (2,3,18)
			SeqAnimation = 0
			LightsequenceAnimation.Enabled = True
	If CurrentMissionFlag(CurrentPlayer) = 1  Or Knight_challenge_flag = True Or CatapultModeFlag = True or WarHurryFlag = True Then	
		If L66State(CurrentPlayer) = 2 Then
			MissionHit 45
			SW60Timer=3000
		End If
	End If
	If CurrentMissionFlag(CurrentPlayer) = 0 And Knightlamp(CurrentPlayer) = 12 And BallsOnPlayfield = 1 And CatapultModeFlag = False And Knight_challenge_flag = False Then
		CurrentMissionFlag(CurrentPlayer) = 2	
		BlackKnightRetro(CurrentPlayer) = BlackKnightRetro(CurrentPlayer) + 1
		Video_Black_Knight_Retro
		If BlackKnightRetro(CurrentPlayer) = 1 Then
			SW60Timer=5000
			MusicCheck 45
		End If
		If BlackKnightRetro(CurrentPlayer) = 3 Then
			AddTimeForMission(CurrentPlayer) = MissionTime
			SW60Timer=26500
			MusicCheck 46
		End If
	End If
timer003.interval=SW60Timer
timer003.Enabled = True

end sub

'******MULTIBALL*********

sub timer002_Timer()
	timer002.Enabled = False
	kicker006.destroyball
	Kicker007.createball
	kicker007.kick  90,2
DOF 119, DOFPulse
end Sub

sub timer003_Timer()
	timer003.Enabled = False
	SW45.destroyball
	playsound "ballrelease"
	CastleVUK2.createball
	'CastleVUK2.kick  90,10
	playsound "fx_plastichit"
'	CountSW45used(CurrentPlayer) = CountSW45used(CurrentPlayer) + 1
'	If CountSW45used(CurrentPlayer) > 3 Then
'		SW41.IsDropped=False
'		CountSW45used(CurrentPlayer) = 0
'	End If
	If CurrentMissionFlag(CurrentPlayer) = 0 And Knight_challenge_flag = False And CatapultModeFlag = False Then
		SW41.IsDropped=False
		SW41Dropped = False
		playsound "fx_plastichit"
	End If
	Flash_lamp.Enabled=True

	If Knight_challenge_flag = True Or CatapultModeFlag = True Then
		CountSW45used(CurrentPlayer) = CountSW45used(CurrentPlayer) + 1
		If CountSW45used(CurrentPlayer) = 1 Then
			L70.State=1
			Video_Add_Ball_Ready
		End If
		If CountSW45used(CurrentPlayer) > 1 Then
			L70.State=0
			Video_Add_Ball
			kickball
			CountSW45used(CurrentPlayer) = 0
		End If
	End If	
'timer004.Enabled = False

'	playsound "ballrelease"
	CastleVUK2.kick 295, 200, 1.56
		DOF 121, DOFPulse
	AddScore 10000
	BaseBonus = BaseBonus + 1800

end Sub



'********************************************
'*********SPINNERS****************************
'********************************************

Sub SW47_hit()
	c=c+1	
	addscore 1000:L70.state=1
	BaseBonus = BaseBonus + 1800
	if c=2 then L69.state=1 end If
	if c=3 then light026.state=2 end If
	playsound "fx1"	
end Sub

Sub SW47()
	c=c+1	
	addscore 1000:L70.state=1
	BaseBonus = BaseBonus + 1800
	if c=2 then L69.state=1 end If
	if c=3 then light026.state=2 end If
	playsound "fx1"	
end Sub


sub timer3_Timer()
Timer2.Enabled=False
end Sub

Sub BouleAfterHit_Timer()
	If CatapultModeFlag = True Then
		BouleAfterHit.Enabled = False
		StopBouleFlag = False
		timer2.interval = 5
		timer2.Enabled = True	
	Else
		'StopBouleFlag = True           ' The previous condition replace this!
	End If
End Sub

Sub boule_hit()
	OrbitFlag = False
	OrbitStatus.Enabled = True	

'	If CurrentMissionFlag(CurrentPlayer) = 0 Then
		Select Case Int(Rnd*20)+1
			Case 1 : audioknight = "Sound-0x0373.mp3":SpeakTime = 4235	'Super Feats is Boosted
			Case 2 : audioknight = "Sound-0x043C.mp3":SpeakTime = 3944	'Super Feats is Boosted
			Case 3 : audioknight = "Sound-0x0187.mp3":SpeakTime = 1617 	'Super Feats is Boosted
			Case 4 : audioknight = "Sound-0x018C.mp3":SpeakTime = 922	'Super Feats is Boosted
			Case 5 : audioknight = "Sound-0x018D.mp3":SpeakTime = 2684	'Super Feats is Boosted
			Case 6 : audioknight = "Sound-0x0198.mp3":SpeakTime = 1811 	'Super Feats is Boosted
			Case 7 : audioknight = "Sound-0x0199.mp3":SpeakTime = 1347	'Super Feats is Boosted
			Case 8 : audioknight = "Sound-0x01C1.mp3":SpeakTime = 1464	'Super Feats is Boosted
			Case 9 : audioknight = "Sound-0x01D0.mp3":SpeakTime = 1914	'Super Feats is Boosted
			Case 10 : audioknight = "Sound-0x01D4.mp3":SpeakTime = 1393	'Super Feats is Boosted
			Case 11 : audioknight = "Sound-0x01E4.mp3":SpeakTime = 1380	'Super Feats is Boosted
			Case 12 : audioknight = "Sound-0x01EE.mp3":SpeakTime = 2940	'Super Feats is Boosted
			Case 13 : audioknight = "Sound-0x0207.mp3":SpeakTime = 1361	'Super Feats is Boosted
'/**/'
			Case 14 : audioknight = "Sound-0x027F.mp3":SpeakTime = 2252'Super Feats is Boosted
			Case 15 : audioknight = "Sound-0x0307.mp3":SpeakTime = 2433	'Super Feats is Boosted
			Case 16 : audioknight = "Sound-0x0326.mp3":SpeakTime = 2704 'Super Feats is Boosted
			Case 17 : audioknight = "Sound-0x0337.mp3":SpeakTime = 2161	'Super Feats is Boosted
			Case 18 : audioknight = "Sound-0x0427.mp3":SpeakTime = 2303	'Super Feats is Boosted
			Case 19 : audioknight = "Sound-0x045D.mp3":SpeakTime = 2074	'Super Feats is Boosted
			Case 20 : audioknight = "Sound-0x0485.mp3":SpeakTime = 2009	'Super Feats is Boosted
		End Select
		LightEyesBK
		playmedia audioknight,"audioknight",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'	End If



	PlaySoundAt "fx_metalhit", ActiveBall
	If CurrentMissionFlag(CurrentPlayer) = 0 And BlackKnightRetro(CurrentPlayer) <> 3 And CatapultModeFlag = False Then
		NumberOfHitForStartMission(CurrentPlayer) = NumberOfHitForStartMission(CurrentPlayer) + 1
	End If
	If ShieldIsReadyToActivateMission(CurrentPlayer) = True Then
		BouleHited = BouleHited +1
	End If
	


'	StopBouleFlag = True
'	StopBouleAngle = 90
'	StopRotating.interval=100
'	StopRotating.Enabled=True
	If CatapultModeFlag = True or BlackKnightRetro(CurrentPlayer) = 3 Then
		StopBouleAngle = 90
		StopRotating.interval = 100
		StopRotating.Enabled=True
		MissionHit 51
		BouleAfterHit.interval = 4000
		BouleAfterHit.Enabled = True
	ElseIf CatapultModeFlag = False Then
		ActionWhenHitOneOfThreeWay
'		StopBouleAngle = 0
'		StopRotating.Enabled=True
'		MissionHit 51
	End If
	If CurrentMissionFlag(CurrentPlayer) = 1  Or Knight_challenge_flag = True or WarHurryFlag = True Then
		If L79State(CurrentPlayer) = 2 Then
			StopBouleAngle = 90
			StopRotating.Enabled=True
			MissionHit 51
		End If
	End If
End Sub

sub timer2_Timer()
	DOF 606, DOFOn
	BouleAngle = BouleAngle + 5
	If BouleAngle = 180 Then
		boule.collidable=True
	End If 

	If BouleAngle = 85 Then
		boule.collidable=False
	End If 
	If BouleAngle = 265 Then
		boule.collidable=False
	End If 

	If BouleAngle = 360 Then
		BouleAngle = 0
		boule.collidable=True
	End If 
	boule.rotx=BouleAngle
	If BouleAngle = StopBouleAngle Then
		If StopBouleFlag = True Then
			timer2.Enabled=False
			DOF 606, DOFOff
			StopSound "Sound-0x0111"
			StopSound "Sound-0x0111"
		End If
	End If
end Sub

Sub StopRotating_timer()
	istoprotating=istoprotating+1
	timer2.interval=15
	timer2.Enabled=True
	StopRotating.Enabled=False
	StopBouleFlag = True
	StopSound "Sound-0x0111"
End Sub

'********************************************
'*********BUMPERS****************************
'********************************************



sub SW70_hit()
	If SuperPops = True Then
		AddScoreChestJackpot = AddScoreChestJackpot + AddScoreSuperPops
	End If
	playsound SoundFXDOF("fx_bumper1",107,DOFPulse,DOFContactors)
	DOF 400, DOFPulse			  
	addscore 10470
	BaseBonus = BaseBonus + 1800
	If CurrentMissionFlag(CurrentPlayer) = 0 Then
		AnimBumpSW70Hit
	End If
	d=d+1
	if d=50 then addscore 20000 end if
	if d=100 then addscore 50000 end if
end Sub

sub SW71_hit()
	If SuperPops = True Then
		AddScoreChestJackpot = AddScoreChestJackpot + AddScoreSuperPops
	End If
	playsound SoundFXDOF("fx_bumper3",109,DOFPulse,DOFContactors)
	DOF 401, DOFPulse
	addscore 10470
	BaseBonus = BaseBonus + 1800
	If CurrentMissionFlag(CurrentPlayer) = 0 Then
		AnimBumpSW71Hit
	End If
	d=d+1
	if d=50 then addscore 20000 end if
	if d=100 then addscore 50000 end if
end Sub

sub SW72_hit()
	If SuperPops = True Then
		AddScoreChestJackpot = AddScoreChestJackpot + AddScoreSuperPops
	End If
	playsound SoundFXDOF("fx_bumper2",108,DOFPulse,DOFContactors)
	DOF 402, DOFPulse
	addscore 10470
	BaseBonus = BaseBonus + 1800
	If CurrentMissionFlag(CurrentPlayer) = 0 Then
		AnimBumpSW72Hit
	End If
	d=d+1
	if d=50 then addscore 20000 end if
	if d=100 then addscore 50000 end if
end Sub



	Sub bump
		inbumps = 1
		pbumps(CurrentPlayer) = pbumps(currentplayer) + 1
		totalbumps(CurrentPlayer) = totalbumps(CurrentPlayer) + 1
		If totalbumps(CurrentPlayer) < 75 Then
			PuPlayer.LabelSet pBackglass,"deatheaternum",75 - totalbumps(CurrentPlayer),1,"{'mt':2,'color':2477823, 'size': 4, 'xpos': 17.1, 'xalign': 1, 'ypos': 88.8, 'yalign': 1}"
		Else
			PuPlayer.LabelSet pBackglass,"deatheaternum",0,1,"{'mt':2,'color':2477823, 'size': 4, 'xpos': 17.1, 'xalign': 1, 'ypos': 88.8, 'yalign': 1}"
			checkmerlinsecond
		end if
		FlashForMs l22, 1000, 50, 0:FlashForMs l20f2, 1000, 50, 0
		'vpmtimer.addtimer 1000, "outofbumps '"
		outofbumps
		checkspell
		'setspellname
	end Sub

	sub checkmerlinsecond
		if merlinsecond(CurrentPlayer) = 0 Then
		pt7(CurrentPlayer) = 1
			t7.state = 1
			merlinsecond(CurrentPlayer) = 1
		end if
	end sub

	sub checkspell
		dim bumpsneeded
		bumpsneeded = 0
		select case bumplvl(CurrentPlayer)
			case 0:bumpsneeded = 25
			case 1:bumpsneeded = 55
			case 2:bumpsneeded = 75
			case 3:bumpsneeded = 95
			case 4:bumpsneeded = 105
			case 5:bumpsneeded = 125
			case 6:bumpsneeded = 145
			case 7:bumpsneeded = 165
		end Select
		If pbumps(CurrentPlayer) > bumpsneeded Then
'			s22.state = 2
			PuPlayer.LabelSet pBackglass,"spellnum",0,1,"{'mt':2,'color':15066597, 'size': 4, 'xpos': 93.6, 'xalign': 1, 'ypos': 5.4, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"spellnum2",0,1,"{'mt':2,'color':3753717, 'size': 10, 'xpos': 50, 'xalign': 1, 'ypos': 42, 'yalign': 1}"
			'PuPlayer.playlistplayex pCallouts,"audiocallouts","get to the book of spells.mp3",vovol,1:pupDMDDisplay "-","Video Mode^is Lit",dmdnote,3,0,10
'			pupDMDDisplay "-","MiniGame^Ready",dmdnote,3,0,1
		Else
			PuPlayer.LabelSet pBackglass,"spellnum",bumpsneeded - pbumps(CurrentPlayer),1,"{'mt':2,'color':15066597, 'size': 4, 'xpos': 93.6, 'xalign': 1, 'ypos': 5.4, 'yalign': 1}"
			PuPlayer.LabelSet pBackglass,"spellnum2",bumpsneeded - pbumps(CurrentPlayer),1,"{'mt':2,'color':3753717, 'size': 10, 'xpos': 50, 'xalign': 1, 'ypos': 42, 'yalign': 1}"
		End If
		
	end Sub

	sub outofbumps
		inbumps = 0
	end Sub

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  FLIPPER LANES
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' 
Sub giflash
	dim a
	For each a in GI
		FlashForMs a, 1000, 150, 1
	Next
End Sub

sub kickball
	'GiOn
	If bMultiBallMode = false Then
		if inamode = 0 Then
			'popright
		end If
	End If
	'flasherspop red,"leftkick"
	'vpmtimer.addtimer 1000, "actualkick '"
	actualkick.enabled=True
end Sub

sub actualkick_timer
	actualkick.enabled=False
	'kicker1.CreateBall
	'PlaySoundAt SoundFXDOF("mbpc-popper",116,DOFPulse,DOFContactors), kicker1
	'DOF 115, DOFPulse 'Strobe
	'kicker1.kick 82, 32, 0
	AutoPlunger.createball
	AutoPlunger.kick 360,45
	playsound SoundFXDOF("ballrelease", 110, DOFPulse, DOFContactors)
	quickcount
end Sub


'****************
'     MULTIBALL
'****************
	
sub doublekick_timer
	doublekick.enabled=False
	'kicker1.CreateBall
	'PlaySoundAt SoundFXDOF("mbpc-popper",116,DOFPulse,DOFContactors), kicker1
	'DOF 115, DOFPulse 'Strobe
	AutoPlunger.createball
	AutoPlunger.kick 360,45
	playsound SoundFXDOF("ballrelease", 110, DOFPulse, DOFContactors)
	quickcount
end Sub
	
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  Minigame
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' 



		'*************************
		' PUP MINI GAME (videomode)
		' create a timer (disable default) PuPGameTimer (interval 300)
		' when you want to start game call PuPGameStartMiniGame
		' look at PuPMiniGameEnd to do something when gameover.
		' game music be called PuPMiniGame.exe inside ofr MiniGame folder of puppack!
		' see sample of key_down and key_up in table script!
		'*************************

		Sub PuPGameStartMiniGame
			if PuPGameRunning Then Exit Sub
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":16, ""EX"": ""MiniGame\\PuPMiniGame.exe"", ""WT"": ""PupMiniGame"", ""RS"":1 , ""TO"":15 , ""WZ"":0 , ""SH"": 1 , ""FT"":""Visual Pinball Player"" }"    
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":3, ""OT"": 0 }"      'this will hide overlay if applicable
			PuPGameTimeout=-3    'check for timeout  every 500 ms
			PuPGameRunning=true	
			PuPGameTimer.enabled=true
			PuPlayer.playpause pBackglass
		End Sub

		Sub PuPMiniGameEnd(gamescore)
			if PuPGameRunning Then Exit Sub
			inminigame = 0
			PuPlayer.playresume pBackglass
			addscore PuPGameScore
			PuPlayer.LabelSet pBackglass,"mgscore",FormatNumber(PuPGameScore,0),1,""
			playmedia "defensescore.mp4","videoscenes",pBackglass,"cineon",4000,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
			'msgbox "mini score "&PuPGameScore
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":3, ""OT"": 1 }"      'this will showsuccess overlay if applicable
			'vpmtimer.addtimer 4000, "clearmgscore '"
			clearmgscore
			'vpmtimer.addtimer 4000, "checkk1lock '"
			checkk1lock
		End Sub

		sub clearmgscore
			PuPlayer.LabelSet pBackglass,"mgscore","",1,""
		end Sub

		Sub PuPGameTimer_Timer()    
			PuPGameTimeout=PuPGameTimeout+1
			PuPGameInfo= PuPlayer.GameUpdate("PupMiniGame", 0 , 0 , "")   '0=game over, 1=game running
			'CHECK GAME OVER
			if PuPGameInfo=0 AND PuPGameTimeOut>12 Then  'gameover if more than 5 seconds passed
			   PuPGameTimer.enabled=false 
			   PupGameRunning=False
			   PuPGameScore= PuPlayer.GameUpdate("PupMiniGame", 6 , 0 , "\MiniGame\gameover.txt")   'grab score from minigame   3=gms 6=godot           
			   'msgbox PuPGameScore  DO something with the score if its over 0!!!
			   PuPMiniGameEnd(PuPGameScore)       
			End If 
		End Sub





'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  UTILITY - BALL FINDER
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' 

	sub ballfinder_timer
		Dim BOT, b
		BOT = GetBalls
		looktimer = looktimer + 1

		' if no balls then no look
		If UBound(BOT) = 1 Then 
			'looktimer = 0
		End If
	
		If bAttractMode = true Then
			looktimer = 0
		end if

		if hsbModeActive = True Then
			looktimer = 0
		end If

		If cineon = 1 Then
			looktimer = 0
		end if

		if inminigame = 1 Then
			looktimer = 0
		end if

		' play the rolling sound for each ball
		For b = 0 to UBound(BOT)
            If BallVel(BOT(b) ) > 1  Then
				looktimer = 0
            End If
		Next

		If ldown = 1 or rdown = 1 Then
			For b = 0 to UBound(BOT)
				if BOT(b).Y > 1775 and BOT(b).Y < 1985 Then			
					if Bot(B).X > 315 and Bot(B).X < 800 then
						'debug.print "being trapped"
						looktimer = 0
					end if
				end if
			Next
		end if

			For b = 0 to UBound(BOT)
				if BOT(b).Y > 1938 and BOT(b).Y < 2077 Then			
					if Bot(B).X > 1014 and Bot(B).X < 1066 then
						'debug.print "in launch lane"
						looktimer = 0
					end if
				end if
			Next

		Select Case looktimer
			case 5:'debug.print "looktimer 5"
			case 10:'debug.print "looktimer 10"
			case 12:bumperrun
			case 15:kickerclear
			case 18:merclear
			case 20:ballstuckoption
		end Select

	end Sub

	sub bumperrun
		Bumper1_Hit
		Nudge 90, 6
		'vpmtimer.addtimer 300, "Bumper3_Hit '"
		Bumper3_Hit
		'vpmtimer.addtimer 600, "Bumper4_Hit '"
		Bumper3_Hit
		'vpmtimer.addtimer 700, "forwardbump '"
		forwardbump
		'vpmtimer.addtimer 900, "Bumper2_Hit '"
		Bumper2_Hit
	end Sub

	sub forwardbump
		Nudge 0, 7
	end sub

	sub kickerclear
		ChestHole.DestroyBall
		'ChestOut.CreateBall
		kicker5.destroyball
		'PlaySoundAt SoundFXDOF("mbpc-vuk",117,DOFPulse,DOFContactors), chestout
		'DOF 115, DOFPulse 'Strobe
		ChestOut.kick 300, 30, 1.56
		dropwall
		ChestOut.kick 300, 30, 1.56
		'kicker1.kick 82, 32, 0
		Kicker1.Kick 300, 30, 1.56
		playsound SoundFXDOF("ballrelease", 110, DOFPulse, DOFContactors)
		'PlaySoundAt SoundFXDOF("mbpc-popper",116,DOFPulse,DOFContactors), kicker1
		'Kicker1.Kick 90,10
		Kicker1.Kick 300, 30, 1.56
		'PlaySoundAt SoundFXDOF("ballrelease", 114, DOFPulse, DOFContactors), BallRelease
		Kicker3.Kick 90, 1
		Kicker2.Kick 90, 1
		mazelock.kick 240, 40, 0
	end sub

	Sub merclear
'		merlock1.kick 160, 30, 0
'		merlock2.kick 160, 30, 0
'		merlock3.kick 160, 30, 0
	end sub

	Sub ballstuckoption
		Kicker1.CreateBall
		'PlaySoundAt SoundFXDOF("ballrelease", 114, DOFPulse, DOFContactors), BallRelease
		'Kicker1.Kick 90,10
		Kicker1.Kick 300, 30, 1.56
		playsound SoundFXDOF("ballrelease", 110, DOFPulse, DOFContactors)
		EnableBallSaver 5
	end Sub



'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  UTILITY - Video Manager & skipper
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
			
	Sub playclear(chan)
'		debug.print "play clear'd " & chan
		if chan = pAudio Then
			'PuPlayer.playlistplayex pAudio,"audioclear","clear.mp3",100,20
			PuPlayer.playstop pAudio
		End If

		if chan = pMusic Then
			'PuPlayer.playlistplayex pMusic,"audioclear","clear.mp3",100,20
			'PuPlayer.playstop pMusic
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 4, ""FN"":11, ""VL"":0 }"
		End If

		if chan = pBackglass Then
			'PuPlayer.playlistplayex pBackglass,"backglass","clear.mp4",0,20
			PuPlayer.playstop pBackglass
		end if 
	End Sub

	'example playmedia "hs.mp3","audiomultiballs",pAudio,"cineon",10000,"",1,1  // (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	Sub playmedia(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	
		if audiolevel = 1 Then
			if channel = pBackglass Then
				audiolevel = vsvol
				currentqueue = " "
			Elseif channel = pCallouts Then
				audiolevel = vovol
			Elseif channel = pMusic Then
				audiolevel = sndtrkvol
			Elseif channel = pAudio Then
				audiolevel = sndknightvol
			Elseif channel = pOvervid Then
				audiolevel = vsvol
			end If
		end If

		if channel = pCallouts Then
			if name = "jackpot.mp3" or name ="" or name = "jackpot.mp3" Then
			else
				if lastvocall = name then exit sub
			end if
		end if
		

		if nextitem = "" Then
			If cinematic = "cineon" Then
				noskipper=1
				'vpmtimer.addtimer length, "nextitems '"
				nextitems
			end If
		Else
			'vpmtimer.addtimer length, "nextitems '"
			nextitems
			currentqueue = nextitem
		end If

		If cinematic = "cineon" Then
			skipped=0
			'PuPlayer.playpause 4 ' stop then resume the music
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 4, ""FN"":11, ""VL"":20 }"
			'vpmtimer.addtimer length, "turnitbackupcine '"
			turnitbackupcine
			'GiOff
			cineon = 1
		end If

		PuPlayer.playlistplayex channel,playlist,name,audiolevel,priority
		If channel = pCallouts Then
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":11, ""VL"":60 }"
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 4, ""FN"":11, ""VL"":60 }"
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 7, ""FN"":11, ""VL"":60 }"
			'vpmtimer.addtimer length, "turnitbackup'"
			turnitbackup
		end If

		If channel = pOvervid Then
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":11, ""VL"":50 }"
			'vpmtimer.addtimer length, "turnitbackupvid'"
			turnitbackupvid
		end If

			if channel = pCallouts Then
				lastvocall=name
			end if
	oldplaylist = playlist
'	If SetBackGroundImportant = True Then
'		SetBackGroundImportant = False
'		PuPlayer.setbackground pBackglass, 2 
'	End If
	end sub

'	Sub SetBackGroundImportant_Timer()
'		SetBackGroundImportant.Enabled = False
'		PuPlayer.setbackground pBackglass, 5
'	End Sub

'Sub SetBackGroundGeneral
'	If OldBackGroundFileIs <> BackGroundFileIs Then
'		'playmedia BackGroundFileIs,BackGroundFolderIs,pBackglass,"cineon",BackGroundLenghtIs,"",1,5  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'		PuPlayer.playlistplayex pBackglass,BackGroundFolderIs,BackGroundFileIs, 0, 0   
'		PuPlayer.setbackground pBackglass, 1 
'		OldBackGroundFileIs = BackGroundFileIs
'	End If
'End Sub


	Sub turnitbackup
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":11, ""VL"":"&vsvol&" }"
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 4, ""FN"":11, ""VL"":"&sndtrkvol&" }"
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 7, ""FN"":11, ""VL"":"&vovol&" }"
'		debug.print "turnitbackup "
		'puplayer.setvolume pMusic sndtrkvol
	End Sub

	Sub turnitbackupvid
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":11, ""VL"":"&vsvol&" }"
'		debug.print "turnitbackupvid "
	End Sub

	Sub turnitbackupcine
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 4, ""FN"":11, ""VL"":"&sndtrkvol&" }"
'		debug.print "turnitbackupcine "
	End Sub

	sub holder
	end sub

	sub nextitems
		if skipped = 0 Then
			noskipper=0
			'PuPlayer.playresume 4
					PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 4, ""FN"":11, ""VL"":"&sndtrkvol&" }"
			gion
			cineon = 0
			looktimer = 0
			execute currentqueue
		Elseif skipped = 1 Then
			'PuPlayer.playresume 4
					PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 4, ""FN"":11, ""VL"":"&sndtrkvol&" }"
			gion
			cineon = 0
			looktimer = 0
			skipped = 3
			execute currentqueue
			currentqueue = " "
		end If
	end Sub



	sub vidskipper_timer
		if cineon = 1  and noskipper = 0 Then
			if ldown = 1 and rdown = 1 Then
				nextitems
				skipped = 1
			end If
		end If
	end Sub

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  UTILITY - CINEMATIC SKIPPING
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  

	Sub checkdown
		If ldown + rdown = 2 Then
			skipscene
		End If
	End Sub

	Sub skipscene

	End Sub



'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  UTILITY - Light Runs
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  

	'***** LIGHT RUNS *****
	'(CHAOS)randoms
	'(DIRECTIONS)uup|udown|uleft|uright|diagdl|diagdr|diagul|diagur
	'(SWIPES)middleih|middleiv|middleoh|middleov|stripe1h|hatch1h|hatch1v|hatch2h|hatch2v|stripe1v|stripe2h|stripe2v
	'(SPINS)circlein|circleout|clockleft|clockright|screwl|screwr
	'(CURVES)arcbld|arcblu|arcbrd|arcbru|arctld|arctlu|arctrd|arctru|fanld|fanlu|fanrd|fanru|radarl|radarr|wiperl|wiperr
	'(EXAMPLE)lightrun red,arcbld,1  (SYNTAX)lightrun color,direction,times to run
	Sub lightrun(colorduring,direction,timenum)
		On Error Resume Next
		dim timefornext
		' color setting
		dim a
		for each a in aLights
			SetLightColor a, colorduring, -1
		next

		if spotstatus = 1 Then
			'spotsoff
		end If
	
		if gistatus = 1 Then
			GiOff
		end if

		select case direction
			Case arcbld
				lrseq.UpdateInterval = 5
				timefornext = timenum*1000
				lrseq.Play SeqArcBottomLeftDownOn, 90,timenum,0
				lrseq.Play SeqArcBottomLeftDownOff, 90,timenum,0
			Case arcblu
				lrseq.UpdateInterval = 5
				timefornext = timenum*1000
				lrseq.Play SeqArcBottomLeftUpOn, 90,timenum,0
				lrseq.Play SeqArcBottomLeftUpOff, 90,timenum,0
			Case arcbrd
				lrseq.UpdateInterval = 5
				timefornext = timenum*1000
				lrseq.Play SeqArcBottomRightDownOn, 90,timenum,0
				lrseq.Play SeqArcBottomRightDownOff, 90,timenum,0
			Case arcbru
				lrseq.UpdateInterval = 5
				timefornext = timenum*1000
				lrseq.Play SeqArcBottomRightUpOn, 90,timenum,0
				lrseq.Play SeqArcBottomRightUpOff, 90,timenum,0
			Case arctld
				lrseq.UpdateInterval = 5
				timefornext = timenum*1000
				lrseq.Play SeqArcTopLeftDownOn, 90,timenum,0
				lrseq.Play SeqArcTopLeftDownOff, 90,timenum,0
			Case arctlu
				lrseq.UpdateInterval = 5
				timefornext = timenum*1000
				lrseq.Play SeqArcTopLeftUpOn, 90,timenum,0
				lrseq.Play SeqArcTopLeftUpOff, 90,timenum,0
			Case arctrd
				lrseq.UpdateInterval = 5
				timefornext = timenum*1000
				lrseq.Play SeqArcTopRightDownOn, 90,timenum,0
				lrseq.Play SeqArcTopRightDownOff, 90,timenum,0
			Case arctru
				lrseq.UpdateInterval = 5
				timefornext = timenum*1000
				lrseq.Play SeqArcTopRightUpOn, 90,timenum,0
				lrseq.Play SeqArcTopRightUpOff, 90,timenum,0
			Case circlein
				lrseq.UpdateInterval = 5
				timefornext = timenum*800
				lrseq.Play SeqCircleInOn,50,timenum,0
				lrseq.Play SeqCircleInOff,50,timenum,0
			Case circleout
				lrseq.UpdateInterval = 5
				timefornext = timenum*800
				lrseq.Play SeqCircleOutOn,50,timenum,0
				lrseq.Play SeqCircleOutOff,50,timenum,0
			Case clockleft
				lrseq.UpdateInterval = 2
				timefornext = timenum*800
				lrseq.Play SeqClockLeftOn, 45,timenum,0
				lrseq.Play SeqClockLeftOff, 45,timenum,0
			Case clockright
				lrseq.UpdateInterval = 2
				timefornext = timenum*800
				lrseq.Play SeqClockRightOn,45,timenum,0
				lrseq.Play SeqClockRightOff,45,timenum,0
			Case diagdl
				lrseq.UpdateInterval = 3
				timefornext = timenum*800
				lrseq.Play SeqDiagDownLeftOn, 25,timenum,0
				lrseq.Play SeqDiagDownLeftOff, 25,timenum,0
			Case diagdr
				lrseq.UpdateInterval = 3
				timefornext = timenum*800
				lrseq.Play SeqDiagDownRightOn, 25,timenum,0
				lrseq.Play SeqDiagDownRightOff, 25,timenum,0
			Case diagul
				lrseq.UpdateInterval = 3
				timefornext = timenum*800
				lrseq.Play SeqDiagUpLeftOn, 25,timenum,0
				lrseq.Play SeqDiagUpLeftOff, 25,timenum,0
			Case diagur
				lrseq.UpdateInterval = 3
				timefornext = timenum*800
				lrseq.Play SeqDiagUpRightOn, 25,timenum,0
				lrseq.Play SeqDiagUpRightOff, 25,timenum,0
			Case udown
				lrseq.UpdateInterval = 5
				timefornext = timenum*800
				lrseq.Play SeqDownOn, 15,timenum,0
				lrseq.Play SeqDownOff, 15,timenum,0
			Case fanld
				lrseq.UpdateInterval = 3
				timefornext = timenum*800
				lrseq.Play SeqFanLeftDownOn, 30,timenum,0
				lrseq.Play SeqFanLeftDownOff, 30,timenum,0
			Case fanlu
				lrseq.UpdateInterval = 3
				timefornext = timenum*800
				lrseq.Play SeqFanLeftUpOn, 30,timenum,0
				lrseq.Play SeqFanLeftUpOff, 30,timenum,0
			Case fanrd
				lrseq.UpdateInterval = 3
				timefornext = timenum*800
				lrseq.Play SeqFanRightDownOn, 30,timenum,0
				lrseq.Play SeqFanRightDownOff, 30,timenum,0
			Case fanru
				lrseq.UpdateInterval = 3
				timefornext = timenum*800
				lrseq.Play SeqFanRightUpOn, 30,timenum,0
				lrseq.Play SeqFanRightUpOff, 30,timenum,0
			Case hatch1h
				lrseq.UpdateInterval = 9
				timefornext = timenum*800
				lrseq.Play SeqHatch1HorizOn, 25,timenum,0
				lrseq.Play SeqHatch1HorizOff, 25,timenum,0
			Case hatch1v
				lrseq.UpdateInterval = 9
				timefornext = timenum*800
				lrseq.Play SeqHatch1VertOn, 75,timenum,0
				lrseq.Play SeqHatch1VertOff, 75,timenum,0
			Case hatch2h
				lrseq.UpdateInterval = 9
				timefornext = timenum*800
				lrseq.Play SeqHatch2HorizOn, 25,timenum,0
				lrseq.Play SeqHatch2HorizOff, 25,timenum,0
			Case hatch2v
				lrseq.UpdateInterval = 9
				timefornext = timenum*800
				lrseq.Play SeqHatch2VertOn, 75,timenum,0
				lrseq.Play SeqHatch2VertOff, 75,timenum,0
			Case uleft
				lrseq.UpdateInterval = 5
				timefornext = timenum*800
				lrseq.Play SeqLeftOn, 50,timenum,0
				lrseq.Play SeqLeftOff, 50,timenum,0
			Case middleih
				lrseq.UpdateInterval = 12
				timefornext = timenum*700
				lrseq.Play SeqMiddleInHorizOn, 50,timenum,0
				lrseq.Play SeqMiddleInHorizOff, 50,timenum,0
			Case middleiv
				lrseq.UpdateInterval = 12
				timefornext = timenum*700
				lrseq.Play SeqMiddleInVertOn, 50,timenum,0
				lrseq.Play SeqMiddleInVertOff, 50,timenum,0
			Case middleoh
				lrseq.UpdateInterval = 12
				timefornext = timenum*700
				lrseq.Play SeqMiddleOutHorizOn, 50,timenum,0
				lrseq.Play SeqMiddleOutHorizOff, 50,timenum,0
			Case middleov
				lrseq.UpdateInterval = 12
				timefornext = timenum*700
				lrseq.Play SeqMiddleOutVertOn, 50,timenum,0
				lrseq.Play SeqMiddleOutVertOff, 50,timenum,0
			Case radarl
				lrseq.UpdateInterval = 4
				timefornext = timenum*700
				lrseq.Play SeqRadarLeftOn, 45,timenum,0
				lrseq.Play SeqRadarLeftOff, 45,timenum,0
			Case radarr
				lrseq.UpdateInterval = 4
				timefornext = timenum*700
				lrseq.Play SeqRadarRightOn, 45,timenum,0
				lrseq.Play SeqRadarRightOff, 45,timenum,0
			Case randoms
				lrseq.UpdateInterval = 5
				timefornext = timenum*1000
				lrseq.Play SeqRandom,40,,timefornext
			Case uright
				lrseq.UpdateInterval = 5
				timefornext = timenum*800
				lrseq.Play SeqRightOn, 50,timenum,0
				lrseq.Play SeqRightOff, 50,timenum,0
			Case screwl
				lrseq.UpdateInterval = 2
				timefornext = timenum*500
				lrseq.Play SeqScrewLeftOn, 25,timenum,0
				lrseq.Play SeqScrewLeftOff, 25,timenum,0
			Case screwr
				lrseq.UpdateInterval = 2
				timefornext = timenum*500
				lrseq.Play SeqScrewRightOn, 25,timenum,0
				lrseq.Play SeqScrewRightOff, 25,timenum,0
			Case stripe1h
				lrseq.UpdateInterval = 5
				timefornext = timenum*900
				lrseq.Play SeqStripe1HorizOn, 25,timenum,0
				lrseq.Play SeqStripe1HorizOff, 25,timenum,0
			Case stripe1v
				lrseq.UpdateInterval = 5
				timefornext = timenum*900
				lrseq.Play SeqStripe1VertOn, 50,timenum,0
				lrseq.Play SeqStripe1VertOff, 50,timenum,0
			Case stripe2h
				lrseq.UpdateInterval = 5
				timefornext = timenum*900
				lrseq.Play SeqStripe2HorizOn, 25,timenum,0
				lrseq.Play SeqStripe2HorizOff, 25,timenum,0
			Case stripe2v
				lrseq.UpdateInterval = 5
				timefornext = timenum*900
				lrseq.Play SeqStripe2VertOn, 25,timenum,0
				lrseq.Play SeqStripe2VertOff, 25,timenum,0
			Case uup
				lrseq.UpdateInterval = 5
				timefornext = timenum*800
				lrseq.Play SeqUpOn, 15,timenum,0
				lrseq.Play SeqUpOff, 15,timenum,0
			Case wiperl
				lrseq.UpdateInterval = 5
				timefornext = timenum*900
				lrseq.Play SeqWiperLeftOn, 45,timenum,0
				lrseq.Play SeqWiperLeftOff, 45,timenum,0
			Case wiperr
				lrseq.UpdateInterval = 5
				timefornext = timenum*900
				lrseq.Play SeqWiperRightOn, 45,timenum,0
				lrseq.Play SeqWiperRightOff, 45,timenum,0
		end Select


		runninglights = 1
		'vpmtimer.addtimer timefornext, "nolongerrun '"
		nolongerrun

	end Sub

	Sub nolongerrun
		if spotstatus = 0 Then
		end If
	
		if gistatus = 0 Then
			GiOn
		end if
		runninglights = 0
		lrseq.StopPlay
		if bMultiBallMode = 0 and inamode = 0 Then
			relighttable
		end if 
	end Sub




'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  Orbital Scoreboard Code
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X 
	

	'****************************
	' POST SCORES
	'****************************

	Sub SubmitOSBScore
'		pupDMDDisplay "-","Uploading Your^Score to OSB",dmdnote,3,0,10
		On Error Resume Next
		if osbactive = 1 or osbactive = 2 Then
		Dim objXmlHttpMain, Url, strJSONToSend 

		Url = "https://hook.integromat.com/82bu988v9grj31vxjklh2e4s6h97rnu0"

		strJSONToSend = "{""auth"":""" & osbkey &""",""player id"": """ & osbid & """,""player initials"": """ & osbtemp &""",""score"": " & CStr(osbtempscore) & ",""table"":"""& TableName & """,""version"":""" & myVersion & """}"

		Set objXmlHttpMain = CreateObject("Msxml2.ServerXMLHTTP")
		objXmlHttpMain.open "PUT",Url, False
		objXmlHttpMain.setRequestHeader "Content-Type", "application/json"
		objXmlHttpMain.setRequestHeader "application", "application/json"

		objXmlHttpMain.send strJSONToSend
		end if
	End Sub	



	'****************************
	' GET SCORES
	'****************************

	Sub GetScores()
		if osbkey="" then exit sub
		On Error Resume Next
		Dim objXmlHttpMain, Url, strJSONToSend 

		Url = "https://hook.integromat.com/97fgba49iff2ucr2lqy72om9j0cha2wj"

		strJSONToSend = "{""auth"":"""& osbkey &""", ""table"":"""& TableName & """, ""range"":""combo"", ""format"":""csv"", ""number"":""10""}"

		Set objXmlHttpMain = CreateObject("Msxml2.ServerXMLHTTP")
		objXmlHttpMain.open "PUT",Url, False
		objXmlHttpMain.setRequestHeader "Content-Type", "application/json"
		objXmlHttpMain.setRequestHeader "application", "application/json"

		objXmlHttpMain.send strJSONToSend

		worldscores = objXmlHttpMain.responseText
		'vpmtimer.addtimer 3000, "showsuccess '"
		showsuccess
'		debug.print "got the scores"
		splitscores
	End Sub	

	sub emptyscores
		dim i
		For i = 0 to 40
			scorevar(i) = 0
		Next
	End Sub
	emptyscores

	Sub splitscores
		On Error Resume Next
		dim a,x
		a = Split(worldscores,",")
		Dim myNum:myNum = 0
		for each x in a
			myNum = MyNum + 1
			scorevar(MyNum) = x
			'debug.print x
		Next
	end Sub

	sub showsuccess
		pNote "Scoreboard","Updated"
		'pupDMDDisplay "-","Scoreboard^Updated",dmdnote,3,0,10
	end sub


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'********************* START OF PUPDMD FRAMEWORK v1.0 *************************XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'******************** DO NOT MODIFY STUFF BELOW   THIS LINE!!!! ***************XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'******************************************************************************XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'*****   Create a PUPPack within PUPPackEditor for layout config!!!  **********XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'******************************************************************************XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	'
	'
	'  Quick Steps:
	'      1>  create a folder in PUPVideos with Starter_PuPPack.zip and call the folder "yourgame"
	'      2>  above set global variable pGameName="yourgame"
	'      3>  copy paste the settings section above to top of table script for user changes.
	'      4>  on Table you need to create ONE timer only called pupDMDUpdate and set it to 250 ms enabled on startup.
	'      5>  go to your table1_init or table first startup function and call PUPINIT function
	'      6>  Go to bottom on framework here and setup game to call the appropriate events like pStartGame (call that in your game code where needed)...etc
	'      7>  attractmodenext at bottom is setup for you already,  just go to each case and add/remove as many as you want and setup the messages to show.  
	'      8>  Have fun and use pDMDDisplay(xxxx)  sub all over where needed.  remember its best to make a bunch of mp4 with text animations... looks the best for sure!
	'
	'
	'Note:  for *Future Pinball* "pupDMDupdate_Timer()" timer needs to be renamed to "pupDMDupdate_expired()"  and then all is good.
	'       and for future pinball you need to add the follow lines near top
	'Need to use BAM and have com idll enabled.
	'				Dim icom : Set icom = xBAM.Get("icom") ' "icom" is name of "icom.dll" in BAM\Plugins dir
	'				if icom is Nothing then MSGBOX "Error cannot run without icom.dll plugin"
	'				Function CreateObject(className)       
	'   					Set CreateObject = icom.CreateObject(className)   
	'				End Function
	'*************  starts PUP system,  must be called AFTER b2s/controller running so put in last line of table1_init
	Sub PuPInit

	'Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay")   
	'PuPlayer.B2SInit "", pGameName

	if (PuPDMDDriverType=pDMDTypeReal) and (useRealDMDScale=1) Then 
		   PuPlayer.setScreenEx pDMD,0,0,128,32,0  'if hardware set the dmd to 128,32
	End if

	PuPlayer.LabelInit pDMD


	if PuPDMDDriverType=pDMDTypeReal then
	Set PUPDMDObject = CreateObject("PUPDMDControl.DMD") 
	PUPDMDObject.DMDOpen
	PUPDMDObject.DMDPuPMirror
	PUPDMDObject.DMDPuPTextMirror
	PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 1, ""FN"":33 }"             'set pupdmd for mirror and hide behind other pups
	PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 1, ""FN"":32, ""FQ"":3 }"   'set no antialias on font render if real
	END IF


	pSetPageLayouts

	pDMDSetPage(pDMDBlank)   'set blank text overlay page.
	pDMDStartUP    ' firsttime running for like an startup video...

	End Sub 'end PUPINIT



	'PinUP Player DMD Helper Functions

	Sub pDMDLabelHide(labName)
	PuPlayer.LabelSet pDMD,labName,"",0,""   
	end sub




	Sub pDMDScrollBig(msgText,timeSec,mColor)
	PuPlayer.LabelShowPage pDMD,2,timeSec,""
	PuPlayer.LabelSet pDMD,"Splash",msgText,0,"{'mt':1,'at':2,'xps':1,'xpe':-1,'len':" & (timeSec*1000000) & ",'mlen':" & (timeSec*1000) & ",'tt':0,'fc':" & mColor & "}"
	end sub

	Sub pDMDScrollBigV(msgText,timeSec,mColor)
	PuPlayer.LabelShowPage pDMD,2,timeSec,""
	PuPlayer.LabelSet pDMD,"Splash",msgText,0,"{'mt':1,'at':2,'yps':1,'ype':-1,'len':" & (timeSec*1000000) & ",'mlen':" & (timeSec*1000) & ",'tt':0,'fc':" & mColor & "}"
	end sub


	Sub pSplashAddScoreDisplayed(msgText,timeSec,mColor)
'	PuPlayer.LabelSet pBackglass,"CommentDisplayed2",msgText,0,"{'mt':1,'at':1,'fq':250,'len':"& (timeSec*1000) &",'fc':" & mColor & "}"
	PuPlayer.LabelSet pBackglass,"AddScoreDisplayed",msgText,0,"{'mt':1, 'size': 10, 'xpos': 50, 'xalign': 1, 'ypos': 35, 'yalign': 1, 'at':3,'hstart':22,'hend':22,'len':" & (timeSec*1000) & ",'mlen':" & (timeSec*500) & ",'tt':5,'fc':" & mColor & "}"
	end Sub

	Sub pSplashAddScoreDisplayed2(msgText,timeSec,mColor)
'	PuPlayer.LabelSet pBackglass,"CommentDisplayed2",msgText,0,"{'mt':1,'at':1,'fq':250,'len':"& (timeSec*1000) &",'fc':" & mColor & "}"
	PuPlayer.LabelSet pBackglass,"AddScoreDisplayed2",msgText,0,"{'mt':1, 'size': 10, 'xpos': 50, 'xalign': 1, 'ypos': 35, 'yalign': 1, 'at':3,'hstart':22,'hend':22,'len':" & (timeSec*1000) & ",'mlen':" & (timeSec*500) & ",'tt':5,'fc':" & mColor & "}"
	end Sub

	Sub pSplashCommentDisplayed(msgText,timeSec,mColor)
'	PuPlayer.LabelSet pBackglass,"CommentDisplayed2",msgText,0,"{'mt':1,'at':1,'fq':250,'len':"& (timeSec*1000) &",'fc':" & mColor & "}"
	PuPlayer.LabelSet pBackglass,"CommentDisplayed",msgText,0,"{'mt':1, 'size': 9, 'xpos': 50, 'xalign': 1, 'ypos': 35, 'yalign': 1, 'at':3,'hstart':22,'hend':22,'len':" & (timeSec*1000) & ",'mlen':" & (timeSec*500) & ",'tt':5,'fc':" & mColor & "}"
	end Sub

	Sub pSplashCommentDisplayed2(msgText,timeSec,mColor)
'	PuPlayer.LabelSet pBackglass,"CommentDisplayed2",msgText,0,"{'mt':1,'at':1,'fq':250,'len':"& (timeSec*1000) &",'fc':" & mColor & "}"
	PuPlayer.LabelSet pBackglass,"CommentDisplayed2",msgText,0,"{'mt':1, 'size': 9, 'xpos': 50, 'xalign': 1, 'ypos': 35, 'yalign': 1, 'at':3,'hstart':22,'hend':22,'len':" & (timeSec*1000) & ",'mlen':" & (timeSec*500) & ",'tt':5,'fc':" & mColor & "}"
	end Sub


	Sub pSplashMystery(msgText,timeSec,mColor)
		PuPlayer.LabelSet pBackglass,"Mystery",msgText,0,"{'mt':1, 'size': 14, 'xpos': 52, 'xalign': 1, 'ypos': 61, 'yalign': 1, 'at':3,'hstart':21,'hend':20,'len':" & (timeSec*1000) & ",'mlen':" & (timeSec*500) & ",'tt':5,'fc':" & mColor & "}"
	End Sub

	Sub pSplashRandomMystery(msgText,timeSec,mColor)
		PuPlayer.LabelSet pBackglass,"RandomMystery",msgText,0,"{'mt':1, 'size': 10, 'xpos': 52, 'xalign': 1, 'ypos': 42, 'yalign': 1, 'at':3,'hstart':19,'hend':18,'len':" & (timeSec*1000) & ",'mlen':" & (timeSec*500) & ",'tt':5,'fc':" & mColor & "}"
	End Sub


'	Sub pSplashScoreScroll(msgText,timeSec,mColor)
'	PuPlayer.LabelSet pBackglass,"CommentDisplayed2",msgText,0,"{'mt':1,'at':2,'xps':1,'xpe':-1,'len':"& (timeSec*1000) &", 'mlen':"& (timeSec*500) &",'tt':0, 'fc':" & mColor & "}"

'	
'	end Sub

	Sub pDMDSplashScore(msgText,timeSec,mColor)
	PuPlayer.LabelSet pDMD,"MsgScore",msgText,0,"{'mt':1,'at':1,'fq':250,'len':"& (timeSec*1000) &",'fc':" & mColor & "}"
	end Sub

	Sub pDMDSplashScoreScroll(msgText,timeSec,mColor)
	PuPlayer.LabelSet pDMD,"MsgScore",msgText,0,"{'mt':1,'at':2,'xps':1,'xpe':-1,'len':"& (timeSec*1000) &", 'mlen':"& (timeSec*1000) &",'tt':0, 'fc':" & mColor & "}"
	end Sub

	Sub pDMDZoomBig(msgText,timeSec,mColor)  'new Zoom
	PuPlayer.LabelShowPage pDMD,2,timeSec,""
	PuPlayer.LabelSet pDMD,"Splash",msgText,0,"{'mt':1,'at':3,'hstart':5,'hend':80,'len':" & (timeSec*1000) & ",'mlen':" & (timeSec*500) & ",'tt':5,'fc':" & mColor & "}"
	end sub

	Sub pDMDTargetLettersInfo(msgText,msgInfo, timeSec)  'msgInfo = '0211'  0= layer 1, 1=layer 2, 2=top layer3.
	'this function is when you want to hilite spelled words.  Like B O N U S but have O S hilited as already hit markers... see example.
	PuPlayer.LabelShowPage pDMD,5,timeSec,""  'show page 5
	Dim backText
	Dim middleText
	Dim flashText
	Dim curChar
	Dim i
	Dim offchars:offchars=0
	Dim spaces:spaces=" "  'set this to 1 or more depends on font space width.  only works with certain fonts
							  'if using a fixed font width then set spaces to just one space.

	For i=1 To Len(msgInfo)
		curChar="" & Mid(msgInfo,i,1)
		if curChar="0" Then
				backText=backText & Mid(msgText,i,1)
				middleText=middleText & spaces
				flashText=flashText & spaces          
				offchars=offchars+1
		End If
		if curChar="1" Then
				backText=backText & spaces
				middleText=middleText & Mid(msgText,i,1)
				flashText=flashText & spaces
		End If
		if curChar="2" Then
				backText=backText & spaces
				middleText=middleText & spaces
				flashText=flashText & Mid(msgText,i,1)
		End If   
	Next 

	if offchars=0 Then 'all litup!... flash entire string
	   backText=""
	   middleText=""
	   FlashText=msgText
	end if  

	PuPlayer.LabelSet pDMD,"Back5"  ,backText  ,1,""
	PuPlayer.LabelSet pDMD,"Middle5",middleText,1,""
	PuPlayer.LabelSet pDMD,"Flash5" ,flashText ,0,"{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) & "}"   
	end Sub


	Sub pDMDSetPage(pagenum)    
		PuPlayer.LabelShowPage pDMD,pagenum,0,""   'set page to blank 0 page if want off
		PDMDCurPage=pagenum
	end Sub

	Sub pHideOverlayText(pDisp)
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "& pDisp &", ""FN"": 34 }"             'hideoverlay text during next videoplay on DMD auto return
	end Sub



	Sub pDMDShowLines3(msgText,msgText2,msgText3,timeSec)
	Dim vis:vis=1
	if pLine1Ani<>"" Then vis=0
	PuPlayer.LabelShowPage pDMD,3,timeSec,""
	PuPlayer.LabelSet pDMD,"Splash3a",msgText,vis,pLine1Ani
	PuPlayer.LabelSet pDMD,"Splash3b",msgText2,vis,pLine2Ani
	PuPlayer.LabelSet pDMD,"Splash3c",msgText3,vis,pLine3Ani
	end Sub

	Sub pDMDShowLines2(msgText,msgText2,timeSec)
	Dim vis:vis=1
	'msg1=msgText
	'msg2=msgText2
	if pLine1Ani<>"" Then vis=0
	PuPlayer.LabelShowPage pDMD,4,timeSec,""
	'vpmtimer.addtimer 500, "showtext '"
	'dim endtime:endtime=(timesec*1000)-400
	'vpmtimer.addtimer endtime, "hidetext '"
	PuPlayer.LabelSet pDMD,"Splash4a",msgText,vis,pLine1Ani
	PuPlayer.LabelSet pDMD,"Splash4b",msgText2,vis,pLine2Ani
	end Sub

	'need to make a timer and add and remove to it instead of showtext and hide text

	sub showtext
		PuPlayer.LabelSet pDMD,"Splash4a",msg1,1,0
		PuPlayer.LabelSet pDMD,"Splash4b",msg2,1,0
	end Sub

	sub hidetext
		PuPlayer.LabelSet pDMD,"Splash4a","",1,0
		PuPlayer.LabelSet pDMD,"Splash4b","",1,0
	end Sub

	Sub pDMDShowCounter(msgText,msgText2,msgText3,timeSec)
	Dim vis:vis=1
	if pLine1Ani<>"" Then vis=0
	PuPlayer.LabelShowPage pDMD,6,timeSec,""
	PuPlayer.LabelSet pDMD,"Splash6a",msgText,vis, pLine1Ani
	PuPlayer.LabelSet pDMD,"Splash6b",msgText2,vis,pLine2Ani
	PuPlayer.LabelSet pDMD,"Splash6c",msgText3,vis,pLine3Ani
	end Sub


	Sub pDMDShowBig(msgText,timeSec, mColor)
	Dim vis:vis=1
	if pLine1Ani<>"" Then vis=0
	PuPlayer.LabelShowPage pDMD,2,timeSec,""
	PuPlayer.LabelSet pDMD,"Splash",msgText,vis,pLine1Ani
	end sub


	Sub pDMDShowHS(msgText,msgText2,msgText3,timeSec) 'High Score
	Dim vis:vis=1
	if pLine1Ani<>"" Then vis=0
	PuPlayer.LabelShowPage pDMD,7,timeSec,""
	PuPlayer.LabelSet pDMD,"Splash7a",msgText,vis,pLine1Ani
	PuPlayer.LabelSet pDMD,"Splash7b",msgText2,vis,pLine2Ani
	PuPlayer.LabelSet pDMD,"Splash7c",msgText3,vis,pLine3Ani
	end Sub

	Sub pDMDSetBackFrame(fname)
	  PuPlayer.playlistplayex pDMD,"PUPFrames",fname,0,1    
	end Sub

	Sub pDMDStartBackLoop(fPlayList,fname)
	  PuPlayer.playlistplayex pDMD,fPlayList,fname,0,1
	  PuPlayer.SetBackGround pDMD,1
	end Sub

	Sub pDMDStopBackLoop
	  PuPlayer.SetBackGround pDMD,0
	  PuPlayer.playstop pDMD
	end Sub

	'************************ where all the MAGIC goes,  pretty much call this everywhere  ****************************************
	'*************************                see docs for examples                ************************************************
	'****************************************   DONT TOUCH THIS CODE   ************************************************************
	
	Sub pupDMDDisplay(pEventID, pText, VideoName,TimeSec, pAni,pPriority)

	if dmdnote = notenow Then
		VideoName = dmdver &"-shortnote2.mp4"
	end if
	
	notenow = VideoName	

	DIM curPos
	if pDMDCurPriority>=pPriority then Exit Sub  'if something is being displayed that we don't want interrupted.  same level will interrupt.
	pDMDCurPriority=pPriority
	if timeSec=0 then timeSec=1 'don't allow page default page by accident


	pLine1=""
	pLine2=""
	pLine3=""
	pLine1Ani=""
	pLine2Ani=""
	pLine3Ani=""


	if pAni=1 Then  'we flashy now aren't we
	pLine1Ani="{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) &  "}"  
	pLine2Ani="{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) &  "}"  
	pLine3Ani="{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) &  "}"  
	end If

	curPos=InStr(pText,"^")   'Lets break apart the string if needed
	if curPos>0 Then 
	   pLine1=Left(pText,curPos-1) 
	   pText=Right(pText,Len(pText) - curPos)
	   
	   curPos=InStr(pText,"^")   'Lets break apart the string
	   if curPOS>0 Then
		  pLine2=Left(pText,curPos-1) 
		  pText=Right(pText,Len(pText) - curPos)

		  curPos=InStr("^",pText)   'Lets break apart the string   
		  if curPos>0 Then
			 pline3=Left(pText,curPos-1) 
		  Else 
			if pText<>"" Then pline3=pText 
		  End if 
	   Else 
		  if pText<>"" Then pLine2=pText
	   End if    
	Else 
	  pLine1=pText  'just one line with no break 
	End if


	'lets see how many lines to Show
	pNumLines=0
	if pLine1<>"" then pNumLines=pNumlines+1
	if pLine2<>"" then pNumLines=pNumlines+1
	if pLine3<>"" then pNumLines=pNumlines+1

	if pDMDVideoPlaying and (VideoName="") Then 
				PuPlayer.playstop pDMD
				pDMDVideoPlaying=False
	End if


	if (VideoName<>"") and (useDMDVideos) Then  'we are showing a splash video instead of the text.
		
		PuPlayer.playlistplayex pDMD,"DMDSplash",VideoName,pDMDDefVolume,pPriority  'should be an attract background (no text is displayed)
		pDMDVideoPlaying=true
	end if 'if showing a splash video with no text




	if StrComp(pEventID,"shownum",1)=0 Then              'check eventIDs
		pDMDShowCounter pLine1,pLine2,pLine3,timeSec
	Elseif StrComp(pEventID,"target",1)=0 Then              'check eventIDs
		pDMDTargetLettersInfo pLine1,pLine2,timeSec
	Elseif StrComp(pEventID,"highscore",1)=0 Then              'check eventIDs
		pDMDShowHS pLine1,pLine2,pline3,timeSec
	Elseif (pNumLines=3) Then                'depends on # of lines which one to use.  pAni=1 will flash.
		pDMDShowLines3 pLine1,pLine2,pLine3,TimeSec
	Elseif (pNumLines=2) Then
		pDMDShowLines2 pLine1,pLine2,TimeSec
	Elseif (pNumLines=1) Then
		pDMDShowBig pLine1,timeSec, curLine1Color
	Else
		pDMDShowBig pLine1,timeSec, curLine1Color
	End if

	PriorityReset=TimeSec*1000
	End Sub 'pupDMDDisplay message

	Sub pupDMDupdate_Timer()
		pDMDUpdateScores    

		if PriorityReset>0 Then  'for splashes we need to reset current prioirty on timer
		   PriorityReset=PriorityReset-pupDMDUpdate.interval
		   if PriorityReset<=0 Then 
				pDMDCurPriority=-1            
				if pInAttract then pAttractReset=pAttractBetween ' pAttractNext  call attract next after 1 second HP no attractmode
				pDMDVideoPlaying=false
				End if
		End if

		if pAttractReset>0 Then  'for splashes we need to reset current prioirty on timer
		   pAttractReset=pAttractReset-pupDMDUpdate.interval
		   if pAttractReset<=0 Then 
				pAttractReset=-1            
				if pInAttract then 
					pAttractNextBackglass
					pAttractNext
				End if
			end If
		end if 
	End Sub

	'********************* END OF PUPDMD FRAMEWORK v1.0 *************************
	'******************** DO NOT MODIFY STUFF ABOVE THIS LINE!!!! ***************
	'****************************************************************************

Sub SW48_Hit()
	SaveStateLightturnlight
	SaveStateLight
	animation_start_and_finish_flag = 1
	TurnOffAllRegularLights

	'seq_animation_array = Array (2,3,18)
	seq_animation_array = Array (2,3,7)
	SeqAnimation = 0
	LightsequenceAnimation.Enabled = True
	If CurrentMissionFlag(CurrentPlayer) = 1  Or Knight_challenge_flag = True Or CatapultModeFlag = True or WarHurryFlag = True Then
		If L72State(CurrentPlayer) = 2 And SWNoHit = False Then
			MissionHit 48
			SWNoHit = True
			SW48_SW64_NoHit.Enabled=True
		End If
	End If
End Sub

Sub FireAddScore_Timer()
	AddScore 320
'	BaseBonus = BaseBonus + 320
End Sub


Sub SW47_Spin()
	If SuperSpinner = True Then
		AddScoreChestJackpot = AddScoreChestJackpot + AddScoreSuperSpinner
	End If
	AddScore 12030
	playsound "fx_spinner"
'	BaseBonus = BaseBonus + 12030
End Sub

Sub SW47bis_Hit()
	If RampLightsMove(CurrentPlayer) = "ShieldReadyForMonster"  Or CurrentMissionFlag(CurrentPlayer) = 1 Then
		'No action...
	Else
		MissionRandomTimer.Interval=1
		MissionRandomTimer.Enabled=True
	End If
End Sub

Sub SW55_Hit()
	AddScore 10030
	BaseBonus = BaseBonus + 1800
End Sub


Sub SW41_Hit()
	OrbitFlag = False
	OrbitStatus.Enabled = True
	DOF 605, DOFPulse
	If TimeSuperMode.Enabled = False Then
		ChangeSuperModeColor
		Select Case Int(Rnd*3)+1
			Case 1 : audioknight = "Sound-0x0944.mp3"	'Super Feats is Boosted
			Case 2 : audioknight = "Sound-0x0944.mp3"	'Super Feats is Boosted
			Case 3 : audioknight = "Sound-0x094C.mp3" 	'Super Feats is Boosted
		End Select
		playmedia audioknight,"Audiocomment",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	End If
	If SuperTargets = True Then
		AddScoreChestJackpot = AddScoreChestJackpot + AddScoreSuperTargets
	End If
	AddScore 10030
	BaseBonus = BaseBonus + 1800
	PlaySoundAt "fx_droptarget", ActiveBall
	SW41.IsDropped=True
	SW41Dropped = True
	If CurrentMissionFlag(CurrentPlayer) = 1  Or Knight_challenge_flag = True Or CatapultModeFlag = True or WarHurryFlag = True Then
		If L66State(CurrentPlayer) = 2 Then
			MissionHit 41
		End If
	End If
End Sub


'*************************************************************************************************************
'*************************************************************************************************************
'
'											END
'
'*************************************************************************************************************
'*************************************************************************************************************

Sub Flash_lamp_Timer()
	Flash_lamp.Enabled=False
	L142.State=0
	L143.State=0
	L270.State=1
	L271.State=1
	L272.State=1
End Sub


Sub RightGateTW_Hit()
	If BlackKnightRetro(CurrentPlayer) = 3 And CounOrbitsFlag = True Then
		MissionHit 19
	ElseIf CounOrbitsFlag = True Then
'-------------------------------------------------------------		AddScoreLoopBonus = AddScoreLoopBonus
	End If
	playsound "gate"
	CounOrbitsFlag = True
	OrbitFlag = True
	OrbitStatus.Enabled = True
End Sub

		  
					 
	   
	   
	   
	   

Sub LeftGate_hit()
	playsound "gate"
	Uturn = Uturn + 1
	If Uturn = 8 Then
		VideoExtraBallIsLit
	End If
End Sub

Sub LeftGateTW_Hit()
	If BlackKnightRetro(CurrentPlayer) = 3 And CounOrbitsFlag = True Then
		MissionHit 19
	ElseIf CounOrbitsFlag = True Then
'-------------------------------------------------------------		AddScoreLoopBonus = AddScoreLoopBonus
	End If
	playsound "gate4"
	LeftGateTWcount = LeftGateTWcount + 1
	If SuperIsLit = False Then
		SuperIsLit = True
		playmedia "Sound-0x08FE.mp3","Audiocomment",pAudio,"",0,"",1,3  '(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	End If
	If LeftGateTWcount = 3 Then
		KnightLamp(CurrentPlayer) = KnightLamp(CurrentPlayer) + 1
		If KnightLamp(CurrentPlayer) > 12 Then KnightLamp(CurrentPlayer) = 12 End if
		Video_Knight
	End If
	If CurrentMissionFlag(CurrentPlayer) = 0 Then
		'video_fire_animation
		'compteuri = 901
		'Fireanimation.Enabled = True
		If Knight_challenge_flag = False And CatapultModeFlag = False And WarHurryFlag = False Then
			Video_FireAnimation2
		End If
	End If
	CounOrbitsFlag = True
	OrbitFlag = True
	OrbitStatus.Enabled = True
End Sub

Sub OrbitStatus_Timer()
	If BlackKnightRetro(CurrentPlayer) <> 3 Then
		CounOrbitsFlag = False
	End If
	OrbitStatus.Enabled = False
	If OrbitFlag = True Then
		RightGate.collidable=False
		LeftGate.collidable=False
		L75.State = 1
		L106.State = 1
	Else
		RightGate.collidable=True
		LeftGate.collidable=True
		L75.State = 0
		L106.State = 0
	End If
End Sub

Sub StopBallControl_Hit()
	If BallsOnPlayfield <=1 Then
		StopBallControlFlag= True
	End If
End Sub

Sub SW64_Hit()
	If CurrentMissionFlag(CurrentPlayer) = 1  Or Knight_challenge_flag = True Or CatapultModeFlag = True or WarHurryFlag = True Then
		If L103State(CurrentPlayer) = 2 And SWNoHit = False Then
			MissionHit 64
			SWNoHit = True
			SW48_SW64_NoHit.Enabled=True
		End If
	End If
End Sub

Sub SW48_SW64_NoHit_Timer()
	SWNoHit = False
	SW48_SW64_NoHit.Enabled=False
End Sub

Sub BurningSandsLightMove_Timer()
	If BurningSandsHit = 1 or BurningSandsHit = 2 Then
		BurningSandsCount = BurningSandsCount + 1
		If BurningSandsCount = 13 Then
			BurningSandsHit = 0
		End If
	Else
		BurningSandsCount = 0
		BurningSandsLightMovementNumber = BurningSandsLightMovementNumber + 1
		If BurningSandsLightMovementNumber = 9 Then
			BurningSandsLightMovementNumber = 1
		End If
	End If
	LightChecked3x = 3
	CheckLamp.Enabled=True
End Sub

Sub DrainHited_Timer()
	DrainHited.Enabled = False
	If Tilted = True Then
'		Drainer.Enabled=True				Old Line sub doesn't  exist???
		DrainerCheck.Enabled = True
	End If
	If BallsOnPlayfield <= 0 Then
		DrainerCheck.Enabled = True
	End If
End Sub

Sub UnblockDrainHited_Timer()
	UnblockDrainHited.Enabled = False
	If Tilted = True Then
		ShieldNumberOfMove = 2
		ShieldTimer.interval=250
		ShieldTimer.Enabled=True
		UPLock.collidable=True
		UPLock.TransY=0
		boule.rotx=0
'		PlaySound "Sound-0x0111",-1
		PlaySound "Sound-0x0111", 0, SongVolume
		Timer2.Enabled=True
		Playsound "unblock"
	End If
	If BallsOnPlayfield <= 0 Then
		ShieldNumberOfMove = 2
		ShieldTimer.interval=250
		ShieldTimer.Enabled=True
		UPLock.collidable=True
		UPLock.TransY=0
		boule.rotx=0
'		PlaySound "Sound-0x0111",-1
		PlaySound "Sound-0x0111", 0, SongVolume
		Timer2.Enabled=True
		Playsound "unblock"
	End If
End Sub

Sub BallControlTimer_Timer()
'	If bGameInPlay Then
		DrainHited.Enabled = True
		UnblockDrainHited.Enabled = True
'	End If
End Sub

Sub TriggerRamp001_Hit()
	PlaySoundAt "fx_metalrolling", ActiveBall
End Sub

Sub RightGate_hit()	
	playsound "gate"
	Uturn = Uturn + 1
	If Uturn = 8 Then
		VideoExtraBallIsLit
	End If
End Sub

Sub CastleVUK2_Hit()
	CastleVUK2.kick 295, 200, 1.56
	playsound "ballrelease"
End Sub

Sub TriggerRampEnd001_Hit()
'	PlaySound "fx_ball_drop1"
End Sub

Sub TriggerRampEnd002_Hit()
	PlaySound "fx_ball_drop1"
End Sub


Sub SW51_Hit()
		OrbitFlag = False
	OrbitStatus.Enabled = True
	If RampActivForValidateMission(CurrentPlayer) = True Then
		ShieldIsReadyToActivateMission(CurrentPlayer) = True
		ActionWhenHitOneOfThreeWay
	End If
	If CurrentMissionFlag(CurrentPlayer) = 0 And BlackKnightRetro(CurrentPlayer) <> 3 And CatapultModeFlag = False Then
		NumberOfHitForStartMission(CurrentPlayer) = NumberOfHitForStartMission(CurrentPlayer) + 1
		ActionWhenHitOneOfThreeWay
	End If

	SaveStateLightturnlight
	SaveStateLight
	animation_start_and_finish_flag = 1
	TurnOffAllRegularLights
	seq_animation_array = Array (2,3,18)
	SeqAnimation = 0
	LightsequenceAnimation.Enabled = True
	If CurrentMissionFlag(CurrentPlayer) = 1  Or Knight_challenge_flag = True Or CatapultModeFlag = True or WarHurryFlag = True Then
		If L79State(CurrentPlayer) = 2 Then
			MissionHit 51
		End If
	End If
'kicker002.destroyball
'Kicker003.createball
'kicker003.kick  90,10
addscore 1000
End Sub

Sub LightEyesBK
	'SpeakTime = xxx Must be defined before calling the function LightEyesBK to stop the light in good time
	StopLightEyesBK.interval = SpeakTime
	StopLightEyesBK.Enabled = True
	LBKYL.State = 1
	LBKYR.State = 1
	BL001.State = 2
	BL002.State = 2
	BL003.State = 2
	BL004.State = 2
	BL005.State = 2
End Sub

Sub StopLightEyesBK_Timer()
	StopLightEyesBK.Enabled = False
	LBKYL.State = 0
	LBKYR.State = 0
	BL001.State = 0
	BL002.State = 0
	BL003.State = 0
	BL004.State = 0
	BL005.State = 0
End Sub