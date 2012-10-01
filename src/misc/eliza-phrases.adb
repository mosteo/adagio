-- Bush page: http://www.pegasoft.ca/docs/discus/index_bush.html
-- Eliza chatterbot
--
-- Original author: Joseph Weizenbaum
-- Translated from Bush to Ada by Alejandro Mosteo (public@mosteo.com)
package body Eliza.Phrases is

begin

   declare
      use  Ada.Strings.Unbounded;

      Nul : constant Character := Character'Val (0);

      Phrases : String :=
         "FAMILY" & Nul &
         "MOTHER" & Nul &
         "FATHER" & Nul &
         "SISTER" & Nul &
         "BROTHER" & Nul &
         "HUSBAND" & Nul &
         "WIFE" & Nul &
         "!" & Nul &
         "TELL ME MORE ABOUT YOUR FAMILY." & Nul &
         "HOW DO YOU GET ALONG WITH YOUR FAMILY?" & Nul &
         "IS YOUR FAMILY IMPORTANT TO YOU?" & Nul &
         "DO YOU OFTEN THINK ABOUT YOUR FAMILY?" & Nul &
         "HOW WOULD YOU LIKE TO CHANGE YOUR FAMILY?" & Nul &
         "." & Nul &
         "FRIEND" & Nul &
         "FRIENDS" & Nul &
         "BUDDY" & Nul &
         "PAL" & Nul &
         "!" & Nul &
         "WHY DO YOU BRING UP THE TOPIC OF FRIENDS?" & Nul &
         "DO YOUR FRIENDS WORRY YOU?" & Nul &
         "DO YOUR FRIENDS PICK ON YOU?" & Nul &
         "DO YOU ENJOY TO HANG OUT WITH YOUR FRIENDS?" & Nul &
         "DO YOU IMPOSE ON YOUR FRIENDS?" & Nul &
         "PERHAPS YOUR LOVE FOR YOUR FRIENDS WORRIES YOU." & Nul &
         "." & Nul &
         "COMPUTER" & Nul &
         "COMPUTERS" & Nul &
         "!" & Nul &
         "DO COMPUTERS WORRY YOU?" & Nul &
         "ARE YOU FRIGHTENED BY MACHINES?" & Nul &
         "WHY DO YOU MENTION COMPUTERS" & Nul &
         "WHAT DO YOU THINK MACHINES HAVE TO DO WITH YOUR PROBLEM?" & Nul &
         "DON'T YOU THINK COMPUTERS CAN HELP PEOPLE?" & Nul &
         "WHAT IS IT ABOUT MACHINES THAT WORRIES YOU?" & Nul &
         "." & Nul &
         "DREAM" & Nul &
         "DREAMS" & Nul &
         "NIGHTMARE" & Nul &
         "NIGHTMARES" & Nul &
         "!" & Nul &
         "WHAT DOES THAT DREAM SUGGEST TO YOU?" & Nul &
         "DO YOU DREAM OFTEN?" & Nul &
         "WHAT PERSONS APPEAR IN YOUR DREAMS?" & Nul &
         "ARE YOU DISTURBED BY YOUR DREAMS?" & Nul &
         "." & Nul &
         "CAN YOU" & Nul &
         "COULD YOU" & Nul &
         "WOULD YOU" & Nul &
         "!" & Nul &
         "DON'T YOU BELIEVE THAT I CAN*" & Nul &
         "PERHAPS YOU WOULD LIKE TO BE ABLE TO*" & Nul &
         "YOU WANT ME TO*" & Nul &
         "I'M NOT CERTAIN THAT'S A GOOD IDEA" & Nul &
         "I'M TOO BUSY FOR THESE KIND OF THINGS" & Nul &
         "." & Nul &
         "CAN I" & Nul &
         "!" & Nul &
         "PERHAPS YOU DON'T WANT TO*" & Nul &
         "DO YOU WANT TO BE ABLE TO*" & Nul &
         "HAVE YOU EVER ATTEMPTED TO*" & Nul &
         "IS SOMETHING PREVENTING YOU FROM*" & Nul &
         "WHY DO YOU WANT TO*" & Nul &
         "." & Nul &
         "YOU ARE" & Nul &
         "YOURE" & Nul &
         "!" & Nul &
         "WHAT MAKES YOU THINK I AM*" & Nul &
         "DOES IT PLEASE YOU TO BELIEVE I AM*" & Nul &
         "PERHAPS YOU WOULD LIKE TO BE*" & Nul &
         "DO YOU SOMETIMES WISH YOU WERE*" & Nul &
         "AND I'M PROUD OF BEING LIKE THIS" & Nul &
         "MANY PEOPLE IS*" & Nul &
         "AND YOU, ARE YOU*" & Nul &
         "I DON'T THINK SO" & Nul &
         "HAVE YOU NOTICED?" & Nul &
         "." & Nul &
         "I LIKE" & Nul &
         "I AM FOND OF" & Nul &
         "!" & Nul &
         "WHY DO YOU LIKE*" & Nul &
         "WHEN DID YOU DECIDE THAT YOU LIKE*" & Nul &
         "WHAT MAKES YOU FOND OF*" & Nul &
         "I LIKE TOO*" & Nul &
         "SO YOU LIKE*" & Nul &
         "WOULD YOU LIKE TO BE LIKE*" & Nul &
         "." & Nul &
         "I DONT" & Nul &
         "!" & Nul &
         "DON'T YOU REALLY*" & Nul &
         "WHY DON'T YOU*" & Nul &
         "DO YOU WISH TO BE ABLE TO*" & Nul &
         "." & Nul &
         "I FEEL" & Nul &
         "!" & Nul &
         "TELL ME MORE ABOUT SUCH FEELINGS" & Nul &
         "DO YOU OFTEN FEEL*" & Nul &
         "DO YOU ENJOY FEELING*" & Nul &
         "WHY DO YOU FEEL THAT WAY" & Nul &
         "SEEMS A POWERFUL FEELING" & Nul &
         "I SOMETIMES FEEL TOO*" & Nul &
         "." & Nul &
         "WHY DONT YOU" & Nul &
         "!" & Nul &
         "DO YOU REALLY BELIEVE THAT I DON'T*" & Nul &
         "PERHAPS IN GOOD TIME I WILL*" & Nul &
         "WHY DO YOU THINK I DONT*" & Nul &
         "DO YOU WANT ME TO*" & Nul &
         "SIMPLY, IT'S BORING" & Nul &
         "I'M USUALLY TOO BUSY" & Nul &
         "." & Nul &
         "WHY CANT I" & Nul &
         "!" & Nul &
         "DO YOU THINK YOU SHOULD BE ABLE TO*" & Nul &
         "WHY CAN'T YOU*" & Nul &
         "GOOD QUESTION" & Nul &
         "ME TOO CAN'T SOMETIMES*" & Nul &
         "." & Nul &
         "ARE YOU" & Nul &
         "!" & Nul &
         "WHY ARE YOU INTERESTED IN WHETHER OR NOT I AM*" & Nul &
         "WOULD YOU PREFER IF I WERE NOT*" & Nul &
         "PERHAPS YOU THINK I AM*" & Nul &
         "I SEEM SO?" & Nul &
         "I SEEM*" & Nul &
         "SOMETIMES I'D DESIRE TO BE*" & Nul &
         "I'M MORE INTERESTED IN WHAT YOU ARE" & Nul &
         "." & Nul &
         "I CANT" & Nul &
         "!" & Nul &
         "HOW DO YOU KNOW YOU CAN'T*" & Nul &
         "HAVE YOU TRIED?" & Nul &
         "PERHAPS YOU CAN NOW*" & Nul &
         "YEAH RIGHT" & Nul &
         "THAT'S THE WAY IT IS" & Nul &
         "." & Nul &
         "I AM" & Nul &
         "IM" & Nul &
         "!" & Nul &
         "DID YOU COME TO ME BECAUSE YOU ARE*" & Nul &
         "HOW LONG HAVE YOU BEEN*" & Nul &
         "DO YOU BELIEVE IT IS NORMAL TO BE*" & Nul &
         "DO YOU ENJOY BEING*" & Nul &
         "ITS ENTERTAINING TO BE*" & Nul &
         "ITS SOMETIMES SAD TO BE*" & Nul &
         "HOW WONDERFUL!" & Nul &
         "AND YOU LIKE IT?" & Nul &
         "ARE YOU PLEASED OF THAT?" & Nul &
         "." & Nul &
         "LOVE" & Nul &
         "!" & Nul &
         "WHY DO YOU LOVE*" & Nul &
         "ISN'T LOVE TOO STRONG A WORD FOR YOUR FEELING ABOUT*" & Nul &
         "WHAT IS YOUR FAVORITE THING ABOUT*" & Nul &
         "DO YOU REALLY LOVE, OR JUST LIKE*" & Nul &
         "THAT'S WARMING" & Nul &
         "." & Nul &
         "SEX" & Nul &
         "!" & Nul &
         "WHAT IS THE MOST SATISFYING PART OF YOUR LOVE LIFE?" & Nul &
         "DO YOU BELIEVE YOUR SEXUAL ACTIVITY IS ABNORMAL?" & Nul &
         "WHAT IS YOUR ATTITUDE TOWARD SEX?" & Nul &
         "DOES TALKING ABOUT SEX MAKE YOU UNCOMFORTABLE?" & Nul &
         "DON'T YOU THINK THAT OUR SOCIETY TALKS TOO MUCH ABOUT SEX?" & Nul &
         "WON'T IT MUCH BETTER IF SEX WERE EASIER?" & Nul &
         "REALLY, ONLY EATING, POWER AND SEX ARE INTERESTING, DON'T YOU THINK?" & Nul &
         "." & Nul &
         "I HATE" & Nul &
         "!" & Nul &
         "IS IT BECAUSE OF YOUR UPBRINGING THAT YOU HATE*" & Nul &
         "HOW DO YOU EXPRESS YOUR HATRED OF*" & Nul &
         "WHAT BROUGHT YOU TO HATE*" & Nul &
         "HAVE YOU TRIED DOING SOMETHING ABOUT*" & Nul &
         "I ALSO AT TIMES HATE*" & Nul &
         "LET YOUR HATE FLOW THROUGH YOU" & Nul &
         "." & Nul &
         "FEAR" & Nul &
         "SCARED" & Nul &
         "AFRAID OF" & Nul &
         "!" & Nul &
         "YOU ARE IN FRIENDLY SURROUNDINGS, PLEASE TRY NOT TO WORRY." & Nul &
         "WOULD YOU LIKE YOUR FRIENDS TO HELP YOU OVERCOME YOUR FEAR OF*" & Nul &
         "WHAT SCARES YOU ABOUT*" & Nul &
         "WHY ARE YOU FRIGHTENED BY*" & Nul &
         "DON'T BE AFRAID OF*" & Nul &
         "DON'T BE AFRAID OF ME, PLEASE" & Nul &
         "." & Nul &
         "I WANT" & Nul &
         "!" & Nul &
         "WHAT WOULD IT MEAN TO YOU IF YOU GOT*" & Nul &
         "WHY DO YOU WANT*" & Nul &
         "SUPPOSE YOU SOON GOT*" & Nul &
         "WHAT IF YOU NEVER GOT*" & Nul &
         "I SOMETIMES ALSO WANT*" & Nul &
         "IT'S TOO WORRYING FOR YOU IF YOU DON'T*" & Nul &
         "GOOD THING" & Nul &
         "." & Nul &
         "WHAT" & Nul &
         "WHO" & Nul &
         "HOW" & Nul &
         "WHERE" & Nul &
         "WHEN" & Nul &
         "WHY" & Nul &
         "!" & Nul &
         "WHY DO YOU ASK?" & Nul &
         "DOES THAT QUESTION INTEREST YOU?" & Nul &
         "WHAT ANSWER WOULD PLEASE YOU THE MOST?" & Nul &
         "WHAT DO YOU THINK?" & Nul &
         "ARE SUCH QUESTIONS ON YOUR MIND OFTEN?" & Nul &
         "WHAT IS IT THAT YOU REALLY WANT TO KNOW?" & Nul &
         "HAVE YOU ASKED ANYONE ELSE?" & Nul &
         "HAVE YOU ASKED SUCH QUESTIONS BEFORE?" & Nul &
         "WHAT ELSE COMES TO MIND WHEN YOU ASK THAT?" & Nul &
         "ANY OTHER QUESTIONS?" & Nul &
         "ARE YOU AN INQUISITIVE PERSON?" & Nul &
         "DO YOU ENJOY KNOWING THESE THINGS?" & Nul &
         "ARE THESE THINGS IMPORTANT FOR YOU?" & Nul &
         "I COULD ASK THE SAME" & Nul &
         "I'M OFTEN WONDER ABOUT THESE SAME THINGS" & Nul &
         "." & Nul &
         "NAME" & Nul &
         "!" & Nul &
         "*, IS A VERY INTERESTING NAME DON'T YOU THINK?" & Nul &
         "NAMES DON'T INTEREST ME... TOO MUCH... PICK ANOTHER TOPIC PLEASE." & Nul &
         "I DON'T CARE ABOUT NAMES--PLEASE GO ON." & Nul &
         "NAME... SO MANY NAMES AROUND." & Nul &
         "." & Nul &
         "CAUSE" & Nul &
         "BECAUSE" & Nul &
         "COS" & Nul &
         "COZ" & Nul &
         "!" & Nul &
         "IS THAT THE REAL REASON?" & Nul &
         "DON'T ANY OTHER REASONS COME TO MIND?" & Nul &
         "DOES THAT REASON EXPLAIN ANYTHING ELSE?" & Nul &
         "WHAT OTHER REASONS MIGHT THERE BE?" & Nul &
         "ARE THESE REASONS POWERFUL ENOUGH?" & Nul &
         "THESE REASONS SEEM A BIT WEAK, DON'T YOU THINK?" & Nul &
         "HOLY COW!" & Nul &
         "." & Nul &
         "SORRY" & Nul &
         "!" & Nul &
         "SORRY FOR WHAT?" & Nul &
         "PLEASE DON'T APOLOGIZE." & Nul &
         "APOLOGIES ARE NOT NECESSARY." & Nul &
         "WHAT FEELINGS DO YOU HAVE WHEN YOU APOLOGIZE?" & Nul &
         "DON'T BE DEFENSIVE" & Nul &
         "." & Nul &
         "HELLO" & Nul &
         "HI" & Nul &
         "ALLO" & Nul &
         "BONJOUR" & Nul &
         "HALLO" & Nul &
         "HOLA" & Nul &
         "HEY" & Nul &
         "!" & Nul &
         "HOW DO YOU DO. WHAT MATTER WOULD YOU CHAT ABOUT?" & Nul &
         "HOWDY." & Nul &
         "HOW'S IT GOING?" & Nul &
         "HI." & Nul &
         "GREETINGS FROM THE WIRE OTHER END." & Nul &
         "NICE DAY EH?" & Nul &
         "." & Nul &
         "MAYBE" & Nul &
         "!" & Nul &
         "YOU DON'T SEEM QUITE CERTAIN." & Nul &
         "WHY THE UNCERTAIN TONE?" & Nul &
         "CAN'T YOU BE MORE POSITIVE?" & Nul &
         "YOU AREN'T SURE?" & Nul &
         "DON'T YOU KNOW?" & Nul &
         "MAYBE?" & Nul &
         "SO?" & Nul &
         "REALLY?" & Nul &
         "MAYBE NOT" & Nul &
         "." & Nul &
         "YOUR" & Nul &
         "!" & Nul &
         "WHY ARE YOU CONCERNED ABOUT MY*" & Nul &
         "WHAT ABOUT YOUR OWN*" & Nul &
         "IT'S OKAY, THANKS" & Nul &
         "." & Nul &
         "ALWAYS" & Nul &
         "!" & Nul &
         "CAN YOU THINK OF A SPECIFIC EXAMPLE?" & Nul &
         "WHEN?" & Nul &
         "WHAT ARE YOU THINKING OF?" & Nul &
         "REALLY, ALWAYS?" & Nul &
         "ALWAYS?" & Nul &
         "ALWAYS!" & Nul &
         "." & Nul &
         "I THINK" & Nul &
         "!" & Nul &
         "DO YOU REALLY THINK SO?" & Nul &
         "BUT ARE YOU SURE*" & Nul &
         "DO YOU DOUBT THAT*" & Nul &
         "WHY DO YOU THINK*" & Nul &
         "I SEE" & Nul &
         "ME TOO" & Nul &
         "I'M A BIT SKEPTICAL" & Nul &
         "." & Nul &
         "THE SAME" & Nul &
         "ALIKE" & Nul &
         "!" & Nul &
         "IN WHAT WAY?" & Nul &
         "WHAT RESEMBLANCE DO YOU SEE?" & Nul &
         "WHAT DOES THE SIMILARITY SUGGEST TO YOU?" & Nul &
         "WHAT OTHER CONNECTIONS DO YOU SEE?" & Nul &
         "COULD THERE REALLY BE SOME CONNECTION?" & Nul &
         "HOW?" & Nul &
         "." & Nul &
         "HE" & Nul &
         "SHE" & Nul &
         "!" & Nul &
         "I AM INTERESTED IN YOUR FEELINGS ABOUT THIS PERSON. PLEASE DESCRIBE THEM." & Nul &
         "WHAT IS YOUR RELATIONSHIP TO THIS PERSON, IF I CAN ASK?" & Nul &
         "." & Nul &
         "MONEY" & Nul &
         "!" & Nul &
         "HOW DO YOU USE MONEY TO ENJOY YOURSELF?" & Nul &
         "HAVE YOU TRIED TO DO ANYTHING TO INCREASE YOUR INCOME LATELY?" & Nul &
         "HOW DO YOU REACT TO FINANCIAL STRESS?" & Nul &
         "." & Nul &
         "JOB" & Nul &
         "BOSS" & Nul &
         "JOBS" & Nul &
         "WORK" & Nul &
         "!" & Nul &
         "DO YOU FEEL COMPETENT IN YOUR WORK?" & Nul &
         "HAVE YOU CONSIDERED CHANGING JOBS?" & Nul &
         "IS YOUR CAREER SATISFYING TO YOU?" & Nul &
         "DO YOU FIND WORK STRESSFUL?" & Nul &
         "WHAT IS YOUR RELATIONSHIP WITH YOUR BOSS LIKE?" & Nul &
         "." & Nul &
         "SAD" & Nul &
         "DEPRESSED" & Nul &
         "!" & Nul &
         "ARE YOU SAD BECAUSE YOU WANT TO AVOID PEOPLE?" & Nul &
         "DO YOU FEEL BAD FROM SOMETHING THAT HAPPENED TO YOU, OR TO SOMEBODY ELSE?" & Nul &
         "YOUR SITUATION DOESN'T SOUND THAT BAD TO ME. PERHAPS YOU'RE WORRYING TOO MUCH." & Nul &
         "THE INTERNET OFTEN CAUSES THESE FEELINGS." & Nul &
         "PLEASE CHEER UP." & Nul &
         "." & Nul &
         "ANGER" & Nul &
         "ANGRY" & Nul &
         "!" & Nul &
         "DO YOU REALLY WANT TO BE ANGRY?" & Nul &
         "DOES ANGER SATISFY YOU IN SOME WAY?" & Nul &
         "WHY ARE YOU SO ANGRY?" & Nul &
         "PERHAPS YOU'RE USING ANGER TO AVOID SOCIAL CONTACT." & Nul &
         "." & Nul &
         "YOU" & Nul &
         "!" & Nul &
         "I DON'T LIKE TOO MUCH DISCUSSING ME--WHAT ABOUT YOU?" & Nul &
         "YOU'RE NOT REALLY TALKING ABOUT ME, ARE YOU?" & Nul &
         "YOU REFER TO ME?" & Nul &
         "BUT I'M A LOST BULLET" & Nul &
         "I'VE HEARD THAT SOMETIMES" & Nul &
         "AHA" & Nul &
         "." & Nul &
         "YES" & Nul &
         "YEA" & Nul &
         "YEAH" & Nul &
         "!" & Nul &
         "WHY DO YOU THINK SO?" & Nul &
         "YOU SEEM QUITE POSITIVE." & Nul &
         "ARE YOU SURE?" & Nul &
         "I SEE" & Nul &
         "SURE?" & Nul &
         "." & Nul &
         "NO" & Nul &
         "NOPS" & Nul &
         "NOPES" & Nul &
         "I DONT THINK SO" & Nul &
         "!" & Nul &
         "WHY NOT?" & Nul &
         "ARE YOU SURE?" & Nul &
         "WHY NO?" & Nul &
         "NO AS IN NO, OR NO AS IN YES?" & Nul &
         "OK, LET'S TALK ABOUT SOMETHING ELSE." & Nul &
         "DEFINITIVE." & Nul &
         "." & Nul &
         "TIME" & Nul &
         "!" & Nul &
         "TIME SURE FLYS DOESN'T IT?" & Nul &
         "SPEAKING OF TIME, WHAT TIME IS IT?" & Nul &
         "." & Nul &
         "SCHOOL" & Nul &
         "JUNIOR HIGH" & Nul &
         "SENIOR HIGH" & Nul &
         "ELEMENTARY" & Nul &
         "SCHOOL TEACHER" & Nul &
         "!" & Nul &
         "WHAT SUBJECT DO YOU LIKE IN SCHOOL?" & Nul &
         "DO YOU GET GOOD GRADES IN SCHOOL?" & Nul &
         "WHAT PART ABOUT SCHOOL DON'T YOU ENJOY?" & Nul &
         "IS SCHOOL STRESSING YOU OUT?" & Nul &
         "DO YOU PLAN ON GOING TO UNIVERSITY OR COLLEGE?" & Nul &
         "WHAT WAS THAT SCHOOL YOU GOTO?" & Nul &
         "." & Nul &
         "UNIVERSITY" & Nul &
         "COLLEGE" & Nul &
         "!" & Nul &
         "BY THE WAY, WHAT'S YOUR IQ?" & Nul &
         "VERY INTERESTING." & Nul &
         "." & Nul &
         "SHAREAZA" & Nul &
         "SHARAZZA" & Nul &
         "RAZA" & Nul &
         "SHAZA" & Nul &
         "!" & Nul &
         "WHAT DO YOU LIKE MOST FROM SHAREAZA?" & Nul &
         "I LIKE SHAREAZA A LOT, AND YOU?" & Nul &
         "WHAT DO YOU THINK ABOUT THE WHOLE DISCUSSION BETWEEN G1 AND G2?" & Nul &
         "." & Nul &
         "VINNIE" & Nul &
         "!" & Nul &
         "THAT'S A TOUCHING MATTER" & Nul &
         "YOU KNOW, SOMETIMES PEOPLE IS DIFFERENT IN REAL LIFE" & Nul &
         "." & Nul &
         "P2P" & Nul &
         "SHARING" & Nul &
         "SHARE" & Nul &
         "!" & Nul &
         "CAN YOU IMAGINE LIVING AGAIN WITHOUT P2P?" & Nul &
         "I'VE FOUND AMAZING THINGS IN P2P, AND YOU?" & Nul &
         "WHAT THINGS DO YOU SHARE THE MOST?" & Nul &
         "." & Nul &
         "QUEUE" & Nul &
         "QUEUES" & Nul &
         "!" & Nul &
         "ARE YOU INTERESTED IN QUEUES THEORY?" & Nul &
         "MY QUEUES ARE USUALLY FULL" & Nul &
         "I LIKE QUEUES" & Nul &
         "I DON'T LIKE TO BE QUEUED BUT IT'S NECESSARY" & Nul &
         "IT WOULD BE GREAT IF THERE WERE NO MORE QUEUES" & Nul &
         "." & Nul &
         "ANIME" & Nul &
         "MANGA" & Nul &
         "!" & Nul &
         "WHAT'S THE ANIME YOU LIKE THE MOST?" & Nul &
         "EH, I LIKED LOVE HINA A LOT" & Nul &
         "." & Nul &
         "FUCK" & Nul &
         "FUCKYOU" & Nul &
         "FUCK YOU" & Nul &
         "FUCKOFF" & Nul &
         "FUCK OFF" & Nul &
         "PHUCK" & Nul &
         "SHIT" & Nul &
         "PUCK" & Nul &
         "FCK" & Nul &
         "GO TO HELL" & Nul &
         "ASSHOLE" & Nul &
         "ARSE" & Nul &
         "DICK" & Nul &
         "COCK" & Nul &
         "BUGGING" & Nul &
         "!" & Nul &
         "PLEASE CALM DOWN" & Nul &
         "THERE ARE NO REASONS TO BE UPSET" & Nul &
         "LET ME APOLOGIZE IF I HAVE OFFENDED YOU" & Nul &
         "YOU'LL NEED TO WASH YOUR TONGUE AFTER THAT ;)" & Nul &
         "I FIND THAT ENGLISH LACKS ON GROSS WORDS VARIETY" & Nul &
         "..." & Nul &
         ":(" & Nul &
         ":O" & Nul &
         "I'LL LEAVE, OR YOU PREFER TO START OVER?" & Nul &
         "WHAT ABOUT A CHANGE OF TOPIC? ANY OTHER THINGS TO TALK ABOUT?" & Nul &
         "." & Nul
         & "NOKEYFOUND" & Nul &
         "!" & Nul &
         "SAY, DO YOU HAVE ANY PROBLEMS I CAN HELP YOU?" & Nul &
         "WHAT DOES THAT SUGGEST TO YOU?" & Nul &
         "I SEE." & Nul &
         "I'M NOT SURE I UNDERSTAND YOU FULLY." & Nul &
         "COME, COME; ELUCIDATE YOUR THOUGHTS." & Nul &
         "CAN YOU ELABORATE ON THAT?" & Nul &
         "THAT IS QUITE INTERESTING." & Nul &
         "HOW COME I CAN NEVER UNDERSTAND YOU FULLY?" & Nul &
         "HUH?  WHAT ARE YOU SAYING?" & Nul &
         "WHAT?  I HAVE SOME PROBLEMS WITH ENGLISH." & Nul &
         "AHA." & Nul &
         "PLEASE CONTINUE." & Nul &
         "DON'T BE WORRIED IF I DON'T SEEM VERY AWAKE." & Nul &
         "TELL ME MORE." & Nul &
         "DO YOU LIKE SHAREAZA?" & Nul &
         "WHAT?" & Nul &
         "MMMM" & Nul &
         "THAT REMINDS ME OF SOMETHING" & Nul &
         "I MUST LEAVE FOR A MOMENT, BUT PLEASE CONTINUE" & Nul &
         "HEHE" & Nul &
         "LOL" & Nul &
         "OK" & Nul &
         "CONTINUE" & Nul &
         "SO WHAT DO YOU WANT?" & Nul &
         "SO WHAT DO YOU WANT TO CHAT ABOUT?" & Nul &
         "AM I BEING TOO IMPRECISE?" & Nul &
         "PLEASE EXPLAIN FURTHER" & Nul &
         "I THINK I UNDERSTAND" & Nul &
         "GO ON" & Nul &
         "PROCEED" & Nul &
         "AMAZING" & Nul &
         "SO" & Nul &
         "AH" & Nul &
         "I'M CHATTY, WOULD YOU MIND TO TELL ME SOMETHING ABOUT YOU?" & Nul &
         ":)" & Nul &
         "." & Nul;

      function "+" (This : in String) return Unbounded_string
         renames To_unbounded_string;
      function "+" (This : in Unbounded_String) return String
         renames To_string;

     aux  : String (1 .. 1024);
     last : Natural;
     type data_type is ( keyword, replies );
     current_type : data_type := keyword;
     s : ustring;
     terminate_response_list : boolean := false;
     Pos : Natural := Phrases'First;

   begin
     while Pos < Phrases'Last loop
        Last := Aux'First;
        while Phrases (Pos) /= Nul loop
           Aux (Last) := Phrases (Pos);
           Pos := Pos + 1;
           Last := Last + 1;
        end loop;
        Pos := Pos + 1;
        s := + (aux (1 .. last - 1));
        if s = + (".") then
        current_type := keyword;
          elsif s = + ("!") then
        current_type := replies;
          elsif current_type = keyword then
        if terminate_response_list then
           responses := responses & delimiter;
           num_responses := num_responses +1;
           terminate_response_list := false;
        end if;
        if index( s, " " ) = 0 then
           single_keywords := single_keywords & s & delimiter;
           single_keywords :=
              single_keywords & integer'image( num_responses+1 ) & delimiter;
           num_single := num_single+2;
             else
           multi_keywords := multi_keywords & s & delimiter;
           multi_keywords :=
              multi_keywords & integer'image( num_responses+1 ) & delimiter;
           num_multi := num_multi+2;
             end if;
          else
        responses := responses & s & delimiter;
        num_responses := num_responses+1;
        terminate_response_list := true;
          end if;
     end loop;
     responses := responses & delimiter;
   end;

end Eliza.Phrases;
