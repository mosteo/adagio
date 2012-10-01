------------------------------------------------------------------------
--         (c) 2001, Antonio Duran. All rights reserved               --
--                       aduran@inicia.es                             --
------------------------------------------------------------------------
-- The Ada Cryptographic Framework (ACF) is free software; you can    --
-- redistribute it and/or modify it under terms of the GNU General    --
-- Public License as published by the Free Software Foundation;       --
-- either version 2, or (at your option) any later version.           --
--                                                                    --
-- The ACF is distributed in the hope that it will be useful, but     --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of        --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU   --
-- General Public License for  more details. You should have received --
-- a copy of the GNU General Public License distributed with the ACF; --
-- see file COPYING. If not, write to the Free Software Foundation,   --
-- 59 Temple Place - Suite 330,  Boston, MA 02111-1307, USA.          --
------------------------------------------------------------------------
-- Identification
--    File name         : acf-hash-algorithms-tiger.adb
--    File kind         : Ada package body
--    Author            : Antonio Duran
--    Creation date     : November 27th., 2001
--    Current version   : 1.0
------------------------------------------------------------------------
-- Purpose:
--    Implements the TIGER message digest algorithm.
------------------------------------------------------------------------
-- Portability issues:
-- TBD.
------------------------------------------------------------------------
-- Performance issues:
-- TBD.
------------------------------------------------------------------------
-- Revision history:
--
-- Ver   Who   When     Why
-- 1.0   ADD   11272001 Initial implementation
--
------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with ACF.Exceptions;                use ACF.Exceptions;

package body ACF.Hash.Algorithms.TIGER is

   ---------------------------------------------------------------------
   -- Generic instantiations
   ---------------------------------------------------------------------

   procedure Free is new Ada.Unchecked_Deallocation(
                              TIGER_Context,
                              TIGER_Context_Ptr);

   ---------------------------------------------------------------------
   -- Constant definitions
   ---------------------------------------------------------------------

   --+---[Initial_State]------------------------------------------------
   --|   Initial values for state registers.
   --+------------------------------------------------------------------

   Initial_State                 : constant State_Registers :=
      (
         16#0123456789ABCDEF#,
         16#FEDCBA9876543210#,
         16#F096A5B4C3B2E187#
      );

   --+---[S_Boxes]------------------------------------------------------

   S_Boxes                       : constant array(
                                       Positive range 1 .. 4, Byte)
                                          of Eight_Bytes :=
      (
         (
            16#02AAB17CF7E90C5E#, 16#AC424B03E243A8EC#,
            16#72CD5BE30DD5FCD3#, 16#6D019B93F6F97F3A#,
            16#CD9978FFD21F9193#, 16#7573A1C9708029E2#,
            16#B164326B922A83C3#, 16#46883EEE04915870#,
            16#EAACE3057103ECE6#, 16#C54169B808A3535C#,
            16#4CE754918DDEC47C#, 16#0AA2F4DFDC0DF40C#,
            16#10B76F18A74DBEFA#, 16#C6CCB6235AD1AB6A#,
            16#13726121572FE2FF#, 16#1A488C6F199D921E#,
            16#4BC9F9F4DA0007CA#, 16#26F5E6F6E85241C7#,
            16#859079DBEA5947B6#, 16#4F1885C5C99E8C92#,
            16#D78E761EA96F864B#, 16#8E36428C52B5C17D#,
            16#69CF6827373063C1#, 16#B607C93D9BB4C56E#,
            16#7D820E760E76B5EA#, 16#645C9CC6F07FDC42#,
            16#BF38A078243342E0#, 16#5F6B343C9D2E7D04#,
            16#F2C28AEB600B0EC6#, 16#6C0ED85F7254BCAC#,
            16#71592281A4DB4FE5#, 16#1967FA69CE0FED9F#,
            16#FD5293F8B96545DB#, 16#C879E9D7F2A7600B#,
            16#860248920193194E#, 16#A4F9533B2D9CC0B3#,
            16#9053836C15957613#, 16#DB6DCF8AFC357BF1#,
            16#18BEEA7A7A370F57#, 16#037117CA50B99066#,
            16#6AB30A9774424A35#, 16#F4E92F02E325249B#,
            16#7739DB07061CCAE1#, 16#D8F3B49CECA42A05#,
            16#BD56BE3F51382F73#, 16#45FAED5843B0BB28#,
            16#1C813D5C11BF1F83#, 16#8AF0E4B6D75FA169#,
            16#33EE18A487AD9999#, 16#3C26E8EAB1C94410#,
            16#B510102BC0A822F9#, 16#141EEF310CE6123B#,
            16#FC65B90059DDB154#, 16#E0158640C5E0E607#,
            16#884E079826C3A3CF#, 16#930D0D9523C535FD#,
            16#35638D754E9A2B00#, 16#4085FCCF40469DD5#,
            16#C4B17AD28BE23A4C#, 16#CAB2F0FC6A3E6A2E#,
            16#2860971A6B943FCD#, 16#3DDE6EE212E30446#,
            16#6222F32AE01765AE#, 16#5D550BB5478308FE#,
            16#A9EFA98DA0EDA22A#, 16#C351A71686C40DA7#,
            16#1105586D9C867C84#, 16#DCFFEE85FDA22853#,
            16#CCFBD0262C5EEF76#, 16#BAF294CB8990D201#,
            16#E69464F52AFAD975#, 16#94B013AFDF133E14#,
            16#06A7D1A32823C958#, 16#6F95FE5130F61119#,
            16#D92AB34E462C06C0#, 16#ED7BDE33887C71D2#,
            16#79746D6E6518393E#, 16#5BA419385D713329#,
            16#7C1BA6B948A97564#, 16#31987C197BFDAC67#,
            16#DE6C23C44B053D02#, 16#581C49FED002D64D#,
            16#DD474D6338261571#, 16#AA4546C3E473D062#,
            16#928FCE349455F860#, 16#48161BBACAAB94D9#,
            16#63912430770E6F68#, 16#6EC8A5E602C6641C#,
            16#87282515337DDD2B#, 16#2CDA6B42034B701B#,
            16#B03D37C181CB096D#, 16#E108438266C71C6F#,
            16#2B3180C7EB51B255#, 16#DF92B82F96C08BBC#,
            16#5C68C8C0A632F3BA#, 16#5504CC861C3D0556#,
            16#ABBFA4E55FB26B8F#, 16#41848B0AB3BACEB4#,
            16#B334A273AA445D32#, 16#BCA696F0A85AD881#,
            16#24F6EC65B528D56C#, 16#0CE1512E90F4524A#,
            16#4E9DD79D5506D35A#, 16#258905FAC6CE9779#,
            16#2019295B3E109B33#, 16#F8A9478B73A054CC#,
            16#2924F2F934417EB0#, 16#3993357D536D1BC4#,
            16#38A81AC21DB6FF8B#, 16#47C4FBF17D6016BF#,
            16#1E0FAADD7667E3F5#, 16#7ABCFF62938BEB96#,
            16#A78DAD948FC179C9#, 16#8F1F98B72911E50D#,
            16#61E48EAE27121A91#, 16#4D62F7AD31859808#,
            16#ECEBA345EF5CEAEB#, 16#F5CEB25EBC9684CE#,
            16#F633E20CB7F76221#, 16#A32CDF06AB8293E4#,
            16#985A202CA5EE2CA4#, 16#CF0B8447CC8A8FB1#,
            16#9F765244979859A3#, 16#A8D516B1A1240017#,
            16#0BD7BA3EBB5DC726#, 16#E54BCA55B86ADB39#,
            16#1D7A3AFD6C478063#, 16#519EC608E7669EDD#,
            16#0E5715A2D149AA23#, 16#177D4571848FF194#,
            16#EEB55F3241014C22#, 16#0F5E5CA13A6E2EC2#,
            16#8029927B75F5C361#, 16#AD139FABC3D6E436#,
            16#0D5DF1A94CCF402F#, 16#3E8BD948BEA5DFC8#,
            16#A5A0D357BD3FF77E#, 16#A2D12E251F74F645#,
            16#66FD9E525E81A082#, 16#2E0C90CE7F687A49#,
            16#C2E8BCBEBA973BC5#, 16#000001BCE509745F#,
            16#423777BBE6DAB3D6#, 16#D1661C7EAEF06EB5#,
            16#A1781F354DAACFD8#, 16#2D11284A2B16AFFC#,
            16#F1FC4F67FA891D1F#, 16#73ECC25DCB920ADA#,
            16#AE610C22C2A12651#, 16#96E0A810D356B78A#,
            16#5A9A381F2FE7870F#, 16#D5AD62EDE94E5530#,
            16#D225E5E8368D1427#, 16#65977B70C7AF4631#,
            16#99F889B2DE39D74F#, 16#233F30BF54E1D143#,
            16#9A9675D3D9A63C97#, 16#5470554FF334F9A8#,
            16#166ACB744A4F5688#, 16#70C74CAAB2E4AEAD#,
            16#F0D091646F294D12#, 16#57B82A89684031D1#,
            16#EFD95A5A61BE0B6B#, 16#2FBD12E969F2F29A#,
            16#9BD37013FEFF9FE8#, 16#3F9B0404D6085A06#,
            16#4940C1F3166CFE15#, 16#09542C4DCDF3DEFB#,
            16#B4C5218385CD5CE3#, 16#C935B7DC4462A641#,
            16#3417F8A68ED3B63F#, 16#B80959295B215B40#,
            16#F99CDAEF3B8C8572#, 16#018C0614F8FCB95D#,
            16#1B14ACCD1A3ACDF3#, 16#84D471F200BB732D#,
            16#C1A3110E95E8DA16#, 16#430A7220BF1A82B8#,
            16#B77E090D39DF210E#, 16#5EF4BD9F3CD05E9D#,
            16#9D4FF6DA7E57A444#, 16#DA1D60E183D4A5F8#,
            16#B287C38417998E47#, 16#FE3EDC121BB31886#,
            16#C7FE3CCC980CCBEF#, 16#E46FB590189BFD03#,
            16#3732FD469A4C57DC#, 16#7EF700A07CF1AD65#,
            16#59C64468A31D8859#, 16#762FB0B4D45B61F6#,
            16#155BAED099047718#, 16#68755E4C3D50BAA6#,
            16#E9214E7F22D8B4DF#, 16#2ADDBF532EAC95F4#,
            16#32AE3909B4BD0109#, 16#834DF537B08E3450#,
            16#FA209DA84220728D#, 16#9E691D9B9EFE23F7#,
            16#0446D288C4AE8D7F#, 16#7B4CC524E169785B#,
            16#21D87F0135CA1385#, 16#CEBB400F137B8AA5#,
            16#272E2B66580796BE#, 16#3612264125C2B0DE#,
            16#057702BDAD1EFBB2#, 16#D4BABB8EACF84BE9#,
            16#91583139641BC67B#, 16#8BDC2DE08036E024#,
            16#603C8156F49F68ED#, 16#F7D236F7DBEF5111#,
            16#9727C4598AD21E80#, 16#A08A0896670A5FD7#,
            16#CB4A8F4309EBA9CB#, 16#81AF564B0F7036A1#,
            16#C0B99AA778199ABD#, 16#959F1EC83FC8E952#,
            16#8C505077794A81B9#, 16#3ACAAF8F056338F0#,
            16#07B43F50627A6778#, 16#4A44AB49F5ECCC77#,
            16#3BC3D6E4B679EE98#, 16#9CC0D4D1CF14108C#,
            16#4406C00B206BC8A0#, 16#82A18854C8D72D89#,
            16#67E366B35C3C432C#, 16#B923DD61102B37F2#,
            16#56AB2779D884271D#, 16#BE83E1B0FF1525AF#,
            16#FB7C65D4217E49A9#, 16#6BDBE0E76D48E7D4#,
            16#08DF828745D9179E#, 16#22EA6A9ADD53BD34#,
            16#E36E141C5622200A#, 16#7F805D1B8CB750EE#,
            16#AFE5C7A59F58E837#, 16#E27F996A4FB1C23C#,
            16#D3867DFB0775F0D0#, 16#D0E673DE6E88891A#,
            16#123AEB9EAFB86C25#, 16#30F1D5D5C145B895#,
            16#BB434A2DEE7269E7#, 16#78CB67ECF931FA38#,
            16#F33B0372323BBF9C#, 16#52D66336FB279C74#,
            16#505F33AC0AFB4EAA#, 16#E8A5CD99A2CCE187#,
            16#534974801E2D30BB#, 16#8D2D5711D5876D90#,
            16#1F1A412891BC038E#, 16#D6E2E71D82E56648#,
            16#74036C3A497732B7#, 16#89B67ED96361F5AB#,
            16#FFED95D8F1EA02A2#, 16#E72B3BD61464D43D#,
            16#A6300F170BDC4820#, 16#EBC18760ED78A77A#
         ),
         (
            16#E6A6BE5A05A12138#, 16#B5A122A5B4F87C98#,
            16#563C6089140B6990#, 16#4C46CB2E391F5DD5#,
            16#D932ADDBC9B79434#, 16#08EA70E42015AFF5#,
            16#D765A6673E478CF1#, 16#C4FB757EAB278D99#,
            16#DF11C6862D6E0692#, 16#DDEB84F10D7F3B16#,
            16#6F2EF604A665EA04#, 16#4A8E0F0FF0E0DFB3#,
            16#A5EDEEF83DBCBA51#, 16#FC4F0A2A0EA4371E#,
            16#E83E1DA85CB38429#, 16#DC8FF882BA1B1CE2#,
            16#CD45505E8353E80D#, 16#18D19A00D4DB0717#,
            16#34A0CFEDA5F38101#, 16#0BE77E518887CAF2#,
            16#1E341438B3C45136#, 16#E05797F49089CCF9#,
            16#FFD23F9DF2591D14#, 16#543DDA228595C5CD#,
            16#661F81FD99052A33#, 16#8736E641DB0F7B76#,
            16#15227725418E5307#, 16#E25F7F46162EB2FA#,
            16#48A8B2126C13D9FE#, 16#AFDC541792E76EEA#,
            16#03D912BFC6D1898F#, 16#31B1AAFA1B83F51B#,
            16#F1AC2796E42AB7D9#, 16#40A3A7D7FCD2EBAC#,
            16#1056136D0AFBBCC5#, 16#7889E1DD9A6D0C85#,
            16#D33525782A7974AA#, 16#A7E25D09078AC09B#,
            16#BD4138B3EAC6EDD0#, 16#920ABFBE71EB9E70#,
            16#A2A5D0F54FC2625C#, 16#C054E36B0B1290A3#,
            16#F6DD59FF62FE932B#, 16#3537354511A8AC7D#,
            16#CA845E9172FADCD4#, 16#84F82B60329D20DC#,
            16#79C62CE1CD672F18#, 16#8B09A2ADD124642C#,
            16#D0C1E96A19D9E726#, 16#5A786A9B4BA9500C#,
            16#0E020336634C43F3#, 16#C17B474AEB66D822#,
            16#6A731AE3EC9BAAC2#, 16#8226667AE0840258#,
            16#67D4567691CAECA5#, 16#1D94155C4875ADB5#,
            16#6D00FD985B813FDF#, 16#51286EFCB774CD06#,
            16#5E8834471FA744AF#, 16#F72CA0AEE761AE2E#,
            16#BE40E4CDAEE8E09A#, 16#E9970BBB5118F665#,
            16#726E4BEB33DF1964#, 16#703B000729199762#,
            16#4631D816F5EF30A7#, 16#B880B5B51504A6BE#,
            16#641793C37ED84B6C#, 16#7B21ED77F6E97D96#,
            16#776306312EF96B73#, 16#AE528948E86FF3F4#,
            16#53DBD7F286A3F8F8#, 16#16CADCE74CFC1063#,
            16#005C19BDFA52C6DD#, 16#68868F5D64D46AD3#,
            16#3A9D512CCF1E186A#, 16#367E62C2385660AE#,
            16#E359E7EA77DCB1D7#, 16#526C0773749ABE6E#,
            16#735AE5F9D09F734B#, 16#493FC7CC8A558BA8#,
            16#B0B9C1533041AB45#, 16#321958BA470A59BD#,
            16#852DB00B5F46C393#, 16#91209B2BD336B0E5#,
            16#6E604F7D659EF19F#, 16#B99A8AE2782CCB24#,
            16#CCF52AB6C814C4C7#, 16#4727D9AFBE11727B#,
            16#7E950D0C0121B34D#, 16#756F435670AD471F#,
            16#F5ADD442615A6849#, 16#4E87E09980B9957A#,
            16#2ACFA1DF50AEE355#, 16#D898263AFD2FD556#,
            16#C8F4924DD80C8FD6#, 16#CF99CA3D754A173A#,
            16#FE477BACAF91BF3C#, 16#ED5371F6D690C12D#,
            16#831A5C285E687094#, 16#C5D3C90A3708A0A4#,
            16#0F7F903717D06580#, 16#19F9BB13B8FDF27F#,
            16#B1BD6F1B4D502843#, 16#1C761BA38FFF4012#,
            16#0D1530C4E2E21F3B#, 16#8943CE69A7372C8A#,
            16#E5184E11FEB5CE66#, 16#618BDB80BD736621#,
            16#7D29BAD68B574D0B#, 16#81BB613E25E6FE5B#,
            16#071C9C10BC07913F#, 16#C7BEEB7909AC2D97#,
            16#C3E58D353BC5D757#, 16#EB017892F38F61E8#,
            16#D4EFFB9C9B1CC21A#, 16#99727D26F494F7AB#,
            16#A3E063A2956B3E03#, 16#9D4A8B9A4AA09C30#,
            16#3F6AB7D500090FB4#, 16#9CC0F2A057268AC0#,
            16#3DEE9D2DEDBF42D1#, 16#330F49C87960A972#,
            16#C6B2720287421B41#, 16#0AC59EC07C00369C#,
            16#EF4EAC49CB353425#, 16#F450244EEF0129D8#,
            16#8ACC46E5CAF4DEB6#, 16#2FFEAB63989263F7#,
            16#8F7CB9FE5D7A4578#, 16#5BD8F7644E634635#,
            16#427A7315BF2DC900#, 16#17D0C4AA2125261C#,
            16#3992486C93518E50#, 16#B4CBFEE0A2D7D4C3#,
            16#7C75D6202C5DDD8D#, 16#DBC295D8E35B6C61#,
            16#60B369D302032B19#, 16#CE42685FDCE44132#,
            16#06F3DDB9DDF65610#, 16#8EA4D21DB5E148F0#,
            16#20B0FCE62FCD496F#, 16#2C1B912358B0EE31#,
            16#B28317B818F5A308#, 16#A89C1E189CA6D2CF#,
            16#0C6B18576AAADBC8#, 16#B65DEAA91299FAE3#,
            16#FB2B794B7F1027E7#, 16#04E4317F443B5BEB#,
            16#4B852D325939D0A6#, 16#D5AE6BEEFB207FFC#,
            16#309682B281C7D374#, 16#BAE309A194C3B475#,
            16#8CC3F97B13B49F05#, 16#98A9422FF8293967#,
            16#244B16B01076FF7C#, 16#F8BF571C663D67EE#,
            16#1F0D6758EEE30DA1#, 16#C9B611D97ADEB9B7#,
            16#B7AFD5887B6C57A2#, 16#6290AE846B984FE1#,
            16#94DF4CDEACC1A5FD#, 16#058A5BD1C5483AFF#,
            16#63166CC142BA3C37#, 16#8DB8526EB2F76F40#,
            16#E10880036F0D6D4E#, 16#9E0523C9971D311D#,
            16#45EC2824CC7CD691#, 16#575B8359E62382C9#,
            16#FA9E400DC4889995#, 16#D1823ECB45721568#,
            16#DAFD983B8206082F#, 16#AA7D29082386A8CB#,
            16#269FCD4403B87588#, 16#1B91F5F728BDD1E0#,
            16#E4669F39040201F6#, 16#7A1D7C218CF04ADE#,
            16#65623C29D79CE5CE#, 16#2368449096C00BB1#,
            16#AB9BF1879DA503BA#, 16#BC23ECB1A458058E#,
            16#9A58DF01BB401ECC#, 16#A070E868A85F143D#,
            16#4FF188307DF2239E#, 16#14D565B41A641183#,
            16#EE13337452701602#, 16#950E3DCF3F285E09#,
            16#59930254B9C80953#, 16#3BF299408930DA6D#,
            16#A955943F53691387#, 16#A15EDECAA9CB8784#,
            16#29142127352BE9A0#, 16#76F0371FFF4E7AFB#,
            16#0239F450274F2228#, 16#BB073AF01D5E868B#,
            16#BFC80571C10E96C1#, 16#D267088568222E23#,
            16#9671A3D48E80B5B0#, 16#55B5D38AE193BB81#,
            16#693AE2D0A18B04B8#, 16#5C48B4ECADD5335F#,
            16#FD743B194916A1CA#, 16#2577018134BE98C4#,
            16#E77987E83C54A4AD#, 16#28E11014DA33E1B9#,
            16#270CC59E226AA213#, 16#71495F756D1A5F60#,
            16#9BE853FB60AFEF77#, 16#ADC786A7F7443DBF#,
            16#0904456173B29A82#, 16#58BC7A66C232BD5E#,
            16#F306558C673AC8B2#, 16#41F639C6B6C9772A#,
            16#216DEFE99FDA35DA#, 16#11640CC71C7BE615#,
            16#93C43694565C5527#, 16#EA038E6246777839#,
            16#F9ABF3CE5A3E2469#, 16#741E768D0FD312D2#,
            16#0144B883CED652C6#, 16#C20B5A5BA33F8552#,
            16#1AE69633C3435A9D#, 16#97A28CA4088CFDEC#,
            16#8824A43C1E96F420#, 16#37612FA66EEEA746#,
            16#6B4CB165F9CF0E5A#, 16#43AA1C06A0ABFB4A#,
            16#7F4DC26FF162796B#, 16#6CBACC8E54ED9B0F#,
            16#A6B7FFEFD2BB253E#, 16#2E25BC95B0A29D4F#,
            16#86D6A58BDEF1388C#, 16#DED74AC576B6F054#,
            16#8030BDBC2B45805D#, 16#3C81AF70E94D9289#,
            16#3EFF6DDA9E3100DB#, 16#B38DC39FDFCC8847#,
            16#123885528D17B87E#, 16#F2DA0ED240B1B642#,
            16#44CEFADCD54BF9A9#, 16#1312200E433C7EE6#,
            16#9FFCC84F3A78C748#, 16#F0CD1F72248576BB#,
            16#EC6974053638CFE4#, 16#2BA7B67C0CEC4E4C#,
            16#AC2F4DF3E5CE32ED#, 16#CB33D14326EA4C11#,
            16#A4E9044CC77E58BC#, 16#5F513293D934FCEF#,
            16#5DC9645506E55444#, 16#50DE418F317DE40A#,
            16#388CB31A69DDE259#, 16#2DB4A83455820A86#,
            16#9010A91E84711AE9#, 16#4DF7F0B7B1498371#,
            16#D62A2EABC0977179#, 16#22FAC097AA8D5C0E#
         ),
         (
            16#F49FCC2FF1DAF39B#, 16#487FD5C66FF29281#,
            16#E8A30667FCDCA83F#, 16#2C9B4BE3D2FCCE63#,
            16#DA3FF74B93FBBBC2#, 16#2FA165D2FE70BA66#,
            16#A103E279970E93D4#, 16#BECDEC77B0E45E71#,
            16#CFB41E723985E497#, 16#B70AAA025EF75017#,
            16#D42309F03840B8E0#, 16#8EFC1AD035898579#,
            16#96C6920BE2B2ABC5#, 16#66AF4163375A9172#,
            16#2174ABDCCA7127FB#, 16#B33CCEA64A72FF41#,
            16#F04A4933083066A5#, 16#8D970ACDD7289AF5#,
            16#8F96E8E031C8C25E#, 16#F3FEC02276875D47#,
            16#EC7BF310056190DD#, 16#F5ADB0AEBB0F1491#,
            16#9B50F8850FD58892#, 16#4975488358B74DE8#,
            16#A3354FF691531C61#, 16#0702BBE481D2C6EE#,
            16#89FB24057DEDED98#, 16#AC3075138596E902#,
            16#1D2D3580172772ED#, 16#EB738FC28E6BC30D#,
            16#5854EF8F63044326#, 16#9E5C52325ADD3BBE#,
            16#90AA53CF325C4623#, 16#C1D24D51349DD067#,
            16#2051CFEEA69EA624#, 16#13220F0A862E7E4F#,
            16#CE39399404E04864#, 16#D9C42CA47086FCB7#,
            16#685AD2238A03E7CC#, 16#066484B2AB2FF1DB#,
            16#FE9D5D70EFBF79EC#, 16#5B13B9DD9C481854#,
            16#15F0D475ED1509AD#, 16#0BEBCD060EC79851#,
            16#D58C6791183AB7F8#, 16#D1187C5052F3EEE4#,
            16#C95D1192E54E82FF#, 16#86EEA14CB9AC6CA2#,
            16#3485BEB153677D5D#, 16#DD191D781F8C492A#,
            16#F60866BAA784EBF9#, 16#518F643BA2D08C74#,
            16#8852E956E1087C22#, 16#A768CB8DC410AE8D#,
            16#38047726BFEC8E1A#, 16#A67738B4CD3B45AA#,
            16#AD16691CEC0DDE19#, 16#C6D4319380462E07#,
            16#C5A5876D0BA61938#, 16#16B9FA1FA58FD840#,
            16#188AB1173CA74F18#, 16#ABDA2F98C99C021F#,
            16#3E0580AB134AE816#, 16#5F3B05B773645ABB#,
            16#2501A2BE5575F2F6#, 16#1B2F74004E7E8BA9#,
            16#1CD7580371E8D953#, 16#7F6ED89562764E30#,
            16#B15926FF596F003D#, 16#9F65293DA8C5D6B9#,
            16#6ECEF04DD690F84C#, 16#4782275FFF33AF88#,
            16#E41433083F820801#, 16#FD0DFE409A1AF9B5#,
            16#4325A3342CDB396B#, 16#8AE77E62B301B252#,
            16#C36F9E9F6655615A#, 16#85455A2D92D32C09#,
            16#F2C7DEA949477485#, 16#63CFB4C133A39EBA#,
            16#83B040CC6EBC5462#, 16#3B9454C8FDB326B0#,
            16#56F56A9E87FFD78C#, 16#2DC2940D99F42BC6#,
            16#98F7DF096B096E2D#, 16#19A6E01E3AD852BF#,
            16#42A99CCBDBD4B40B#, 16#A59998AF45E9C559#,
            16#366295E807D93186#, 16#6B48181BFAA1F773#,
            16#1FEC57E2157A0A1D#, 16#4667446AF6201AD5#,
            16#E615EBCACFB0F075#, 16#B8F31F4F68290778#,
            16#22713ED6CE22D11E#, 16#3057C1A72EC3C93B#,
            16#CB46ACC37C3F1F2F#, 16#DBB893FD02AAF50E#,
            16#331FD92E600B9FCF#, 16#A498F96148EA3AD6#,
            16#A8D8426E8B6A83EA#, 16#A089B274B7735CDC#,
            16#87F6B3731E524A11#, 16#118808E5CBC96749#,
            16#9906E4C7B19BD394#, 16#AFED7F7E9B24A20C#,
            16#6509EADEEB3644A7#, 16#6C1EF1D3E8EF0EDE#,
            16#B9C97D43E9798FB4#, 16#A2F2D784740C28A3#,
            16#7B8496476197566F#, 16#7A5BE3E6B65F069D#,
            16#F96330ED78BE6F10#, 16#EEE60DE77A076A15#,
            16#2B4BEE4AA08B9BD0#, 16#6A56A63EC7B8894E#,
            16#02121359BA34FEF4#, 16#4CBF99F8283703FC#,
            16#398071350CAF30C8#, 16#D0A77A89F017687A#,
            16#F1C1A9EB9E423569#, 16#8C7976282DEE8199#,
            16#5D1737A5DD1F7ABD#, 16#4F53433C09A9FA80#,
            16#FA8B0C53DF7CA1D9#, 16#3FD9DCBC886CCB77#,
            16#C040917CA91B4720#, 16#7DD00142F9D1DCDF#,
            16#8476FC1D4F387B58#, 16#23F8E7C5F3316503#,
            16#032A2244E7E37339#, 16#5C87A5D750F5A74B#,
            16#082B4CC43698992E#, 16#DF917BECB858F63C#,
            16#3270B8FC5BF86DDA#, 16#10AE72BB29B5DD76#,
            16#576AC94E7700362B#, 16#1AD112DAC61EFB8F#,
            16#691BC30EC5FAA427#, 16#FF246311CC327143#,
            16#3142368E30E53206#, 16#71380E31E02CA396#,
            16#958D5C960AAD76F1#, 16#F8D6F430C16DA536#,
            16#C8FFD13F1BE7E1D2#, 16#7578AE66004DDBE1#,
            16#05833F01067BE646#, 16#BB34B5AD3BFE586D#,
            16#095F34C9A12B97F0#, 16#247AB64525D60CA8#,
            16#DCDBC6F3017477D1#, 16#4A2E14D4DECAD24D#,
            16#BDB5E6D9BE0A1EEB#, 16#2A7E70F7794301AB#,
            16#DEF42D8A270540FD#, 16#01078EC0A34C22C1#,
            16#E5DE511AF4C16387#, 16#7EBB3A52BD9A330A#,
            16#77697857AA7D6435#, 16#004E831603AE4C32#,
            16#E7A21020AD78E312#, 16#9D41A70C6AB420F2#,
            16#28E06C18EA1141E6#, 16#D2B28CBD984F6B28#,
            16#26B75F6C446E9D83#, 16#BA47568C4D418D7F#,
            16#D80BADBFE6183D8E#, 16#0E206D7F5F166044#,
            16#E258A43911CBCA3E#, 16#723A1746B21DC0BC#,
            16#C7CAA854F5D7CDD3#, 16#7CAC32883D261D9C#,
            16#7690C26423BA942C#, 16#17E55524478042B8#,
            16#E0BE477656A2389F#, 16#4D289B5E67AB2DA0#,
            16#44862B9C8FBBFD31#, 16#B47CC8049D141365#,
            16#822C1B362B91C793#, 16#4EB14655FB13DFD8#,
            16#1ECBBA0714E2A97B#, 16#6143459D5CDE5F14#,
            16#53A8FBF1D5F0AC89#, 16#97EA04D81C5E5B00#,
            16#622181A8D4FDB3F3#, 16#E9BCD341572A1208#,
            16#1411258643CCE58A#, 16#9144C5FEA4C6E0A4#,
            16#0D33D06565CF620F#, 16#54A48D489F219CA1#,
            16#C43E5EAC6D63C821#, 16#A9728B3A72770DAF#,
            16#D7934E7B20DF87EF#, 16#E35503B61A3E86E5#,
            16#CAE321FBC819D504#, 16#129A50B3AC60BFA6#,
            16#CD5E68EA7E9FB6C3#, 16#B01C90199483B1C7#,
            16#3DE93CD5C295376C#, 16#AED52EDF2AB9AD13#,
            16#2E60F512C0A07884#, 16#BC3D86A3E36210C9#,
            16#35269D9B163951CE#, 16#0C7D6E2AD0CDB5FA#,
            16#59E86297D87F5733#, 16#298EF221898DB0E7#,
            16#55000029D1A5AA7E#, 16#8BC08AE1B5061B45#,
            16#C2C31C2B6C92703A#, 16#94CC596BAF25EF42#,
            16#0A1D73DB22540456#, 16#04B6A0F9D9C4179A#,
            16#EFFDAFA2AE3D3C60#, 16#F7C8075BB49496C4#,
            16#9CC5C7141D1CD4E3#, 16#78BD1638218E5534#,
            16#B2F11568F850246A#, 16#EDFABCFA9502BC29#,
            16#796CE5F2DA23051B#, 16#AAE128B0DC93537C#,
            16#3A493DA0EE4B29AE#, 16#B5DF6B2C416895D7#,
            16#FCABBD25122D7F37#, 16#70810B58105DC4B1#,
            16#E10FDD37F7882A90#, 16#524DCAB5518A3F5C#,
            16#3C9E85878451255B#, 16#4029828119BD34E2#,
            16#74A05B6F5D3CECCB#, 16#B610021542E13ECA#,
            16#0FF979D12F59E2AC#, 16#6037DA27E4F9CC50#,
            16#5E92975A0DF1847D#, 16#D66DE190D3E623FE#,
            16#5032D6B87B568048#, 16#9A36B7CE8235216E#,
            16#80272A7A24F64B4A#, 16#93EFED8B8C6916F7#,
            16#37DDBFF44CCE1555#, 16#4B95DB5D4B99BD25#,
            16#92D3FDA169812FC0#, 16#FB1A4A9A90660BB6#,
            16#730C196946A4B9B2#, 16#81E289AA7F49DA68#,
            16#64669A0F83B1A05F#, 16#27B3FF7D9644F48B#,
            16#CC6B615C8DB675B3#, 16#674F20B9BCEBBE95#,
            16#6F31238275655982#, 16#5AE488713E45CF05#,
            16#BF619F9954C21157#, 16#EABAC46040A8EAE9#,
            16#454C6FE9F2C0C1CD#, 16#419CF6496412691C#,
            16#D3DC3BEF265B0F70#, 16#6D0E60F5C3578A9E#
         ),
         (
            16#5B0E608526323C55#, 16#1A46C1A9FA1B59F5#,
            16#A9E245A17C4C8FFA#, 16#65CA5159DB2955D7#,
            16#05DB0A76CE35AFC2#, 16#81EAC77EA9113D45#,
            16#528EF88AB6AC0A0D#, 16#A09EA253597BE3FF#,
            16#430DDFB3AC48CD56#, 16#C4B3A67AF45CE46F#,
            16#4ECECFD8FBE2D05E#, 16#3EF56F10B39935F0#,
            16#0B22D6829CD619C6#, 16#17FD460A74DF2069#,
            16#6CF8CC8E8510ED40#, 16#D6C824BF3A6ECAA7#,
            16#61243D581A817049#, 16#048BACB6BBC163A2#,
            16#D9A38AC27D44CC32#, 16#7FDDFF5BAAF410AB#,
            16#AD6D495AA804824B#, 16#E1A6A74F2D8C9F94#,
            16#D4F7851235DEE8E3#, 16#FD4B7F886540D893#,
            16#247C20042AA4BFDA#, 16#096EA1C517D1327C#,
            16#D56966B4361A6685#, 16#277DA5C31221057D#,
            16#94D59893A43ACFF7#, 16#64F0C51CCDC02281#,
            16#3D33BCC4FF6189DB#, 16#E005CB184CE66AF1#,
            16#FF5CCD1D1DB99BEA#, 16#B0B854A7FE42980F#,
            16#7BD46A6A718D4B9F#, 16#D10FA8CC22A5FD8C#,
            16#D31484952BE4BD31#, 16#C7FA975FCB243847#,
            16#4886ED1E5846C407#, 16#28CDDB791EB70B04#,
            16#C2B00BE2F573417F#, 16#5C9590452180F877#,
            16#7A6BDDFFF370EB00#, 16#CE509E38D6D9D6A4#,
            16#EBEB0F00647FA702#, 16#1DCC06CF76606F06#,
            16#E4D9F28BA286FF0A#, 16#D85A305DC918C262#,
            16#475B1D8732225F54#, 16#2D4FB51668CCB5FE#,
            16#A679B9D9D72BBA20#, 16#53841C0D912D43A5#,
            16#3B7EAA48BF12A4E8#, 16#781E0E47F22F1DDF#,
            16#EFF20CE60AB50973#, 16#20D261D19DFFB742#,
            16#16A12B03062A2E39#, 16#1960EB2239650495#,
            16#251C16FED50EB8B8#, 16#9AC0C330F826016E#,
            16#ED152665953E7671#, 16#02D63194A6369570#,
            16#5074F08394B1C987#, 16#70BA598C90B25CE1#,
            16#794A15810B9742F6#, 16#0D5925E9FCAF8C6C#,
            16#3067716CD868744E#, 16#910AB077E8D7731B#,
            16#6A61BBDB5AC42F61#, 16#93513EFBF0851567#,
            16#F494724B9E83E9D5#, 16#E887E1985C09648D#,
            16#34B1D3C675370CFD#, 16#DC35E433BC0D255D#,
            16#D0AAB84234131BE0#, 16#08042A50B48B7EAF#,
            16#9997C4EE44A3AB35#, 16#829A7B49201799D0#,
            16#263B8307B7C54441#, 16#752F95F4FD6A6CA6#,
            16#927217402C08C6E5#, 16#2A8AB754A795D9EE#,
            16#A442F7552F72943D#, 16#2C31334E19781208#,
            16#4FA98D7CEAEE6291#, 16#55C3862F665DB309#,
            16#BD0610175D53B1F3#, 16#46FE6CB840413F27#,
            16#3FE03792DF0CFA59#, 16#CFE700372EB85E8F#,
            16#A7BE29E7ADBCE118#, 16#E544EE5CDE8431DD#,
            16#8A781B1B41F1873E#, 16#A5C94C78A0D2F0E7#,
            16#39412E2877B60728#, 16#A1265EF3AFC9A62C#,
            16#BCC2770C6A2506C5#, 16#3AB66DD5DCE1CE12#,
            16#E65499D04A675B37#, 16#7D8F523481BFD216#,
            16#0F6F64FCEC15F389#, 16#74EFBE618B5B13C8#,
            16#ACDC82B714273E1D#, 16#DD40BFE003199D17#,
            16#37E99257E7E061F8#, 16#FA52626904775AAA#,
            16#8BBBF63A463D56F9#, 16#F0013F1543A26E64#,
            16#A8307E9F879EC898#, 16#CC4C27A4150177CC#,
            16#1B432F2CCA1D3348#, 16#DE1D1F8F9F6FA013#,
            16#606602A047A7DDD6#, 16#D237AB64CC1CB2C7#,
            16#9B938E7225FCD1D3#, 16#EC4E03708E0FF476#,
            16#FEB2FBDA3D03C12D#, 16#AE0BCED2EE43889A#,
            16#22CB8923EBFB4F43#, 16#69360D013CF7396D#,
            16#855E3602D2D4E022#, 16#073805BAD01F784C#,
            16#33E17A133852F546#, 16#DF4874058AC7B638#,
            16#BA92B29C678AA14A#, 16#0CE89FC76CFAADCD#,
            16#5F9D4E0908339E34#, 16#F1AFE9291F5923B9#,
            16#6E3480F60F4A265F#, 16#EEBF3A2AB29B841C#,
            16#E21938A88F91B4AD#, 16#57DFEFF845C6D3C3#,
            16#2F006B0BF62CAAF2#, 16#62F479EF6F75EE78#,
            16#11A55AD41C8916A9#, 16#F229D29084FED453#,
            16#42F1C27B16B000E6#, 16#2B1F76749823C074#,
            16#4B76ECA3C2745360#, 16#8C98F463B91691BD#,
            16#14BCC93CF1ADE66A#, 16#8885213E6D458397#,
            16#8E177DF0274D4711#, 16#B49B73B5503F2951#,
            16#10168168C3F96B6B#, 16#0E3D963B63CAB0AE#,
            16#8DFC4B5655A1DB14#, 16#F789F1356E14DE5C#,
            16#683E68AF4E51DAC1#, 16#C9A84F9D8D4B0FD9#,
            16#3691E03F52A0F9D1#, 16#5ED86E46E1878E80#,
            16#3C711A0E99D07150#, 16#5A0865B20C4E9310#,
            16#56FBFC1FE4F0682E#, 16#EA8D5DE3105EDF9B#,
            16#71ABFDB12379187A#, 16#2EB99DE1BEE77B9C#,
            16#21ECC0EA33CF4523#, 16#59A4D7521805C7A1#,
            16#3896F5EB56AE7C72#, 16#AA638F3DB18F75DC#,
            16#9F39358DABE9808E#, 16#B7DEFA91C00B72AC#,
            16#6B5541FD62492D92#, 16#6DC6DEE8F92E4D5B#,
            16#353F57ABC4BEEA7E#, 16#735769D6DA5690CE#,
            16#0A234AA642391484#, 16#F6F9508028F80D9D#,
            16#B8E319A27AB3F215#, 16#31AD9C1151341A4D#,
            16#773C22A57BEF5805#, 16#45C7561A07968633#,
            16#F913DA9E249DBE36#, 16#DA652D9B78A64C68#,
            16#4C27A97F3BC334EF#, 16#76621220E66B17F4#,
            16#967743899ACD7D0B#, 16#F3EE5BCAE0ED6782#,
            16#409F753600C879FC#, 16#06D09A39B5926DB6#,
            16#6F83AEB0317AC588#, 16#01E6CA4A86381F21#,
            16#66FF3462D19F3025#, 16#72207C24DDFD3BFB#,
            16#4AF6B6D3E2ECE2EB#, 16#9C994DBEC7EA08DE#,
            16#49ACE597B09A8BC4#, 16#B38C4766CF0797BA#,
            16#131B9373C57C2A75#, 16#B1822CCE61931E58#,
            16#9D7555B909BA1C0C#, 16#127FAFDD937D11D2#,
            16#29DA3BADC66D92E4#, 16#A2C1D57154C2ECBC#,
            16#58C5134D82F6FE24#, 16#1C3AE3515B62274F#,
            16#E907C82E01CB8126#, 16#F8ED091913E37FCB#,
            16#3249D8F9C80046C9#, 16#80CF9BEDE388FB63#,
            16#1881539A116CF19E#, 16#5103F3F76BD52457#,
            16#15B7E6F5AE47F7A8#, 16#DBD7C6DED47E9CCF#,
            16#44E55C410228BB1A#, 16#B647D4255EDB4E99#,
            16#5D11882BB8AAFC30#, 16#F5098BBB29D3212A#,
            16#8FB5EA14E90296B3#, 16#677B942157DD025A#,
            16#FB58E7C0A390ACB5#, 16#89D3674C83BD4A01#,
            16#9E2DA4DF4BF3B93B#, 16#FCC41E328CAB4829#,
            16#03F38C96BA582C52#, 16#CAD1BDBD7FD85DB2#,
            16#BBB442C16082AE83#, 16#B95FE86BA5DA9AB0#,
            16#B22E04673771A93F#, 16#845358C9493152D8#,
            16#BE2A488697B4541E#, 16#95A2DC2DD38E6966#,
            16#C02C11AC923C852B#, 16#2388B1990DF2A87B#,
            16#7C8008FA1B4F37BE#, 16#1F70D0C84D54E503#,
            16#5490ADEC7ECE57D4#, 16#002B3C27D9063A3A#,
            16#7EAEA3848030A2BF#, 16#C602326DED2003C0#,
            16#83A7287D69A94086#, 16#C57A5FCB30F57A8A#,
            16#B56844E479EBE779#, 16#A373B40F05DCBCE9#,
            16#D71A786E88570EE2#, 16#879CBACDBDE8F6A0#,
            16#976AD1BCC164A32F#, 16#AB21E25E9666D78B#,
            16#901063AAE5E5C33C#, 16#9818B34448698D90#,
            16#E36487AE3E1E8ABB#, 16#AFBDF931893BDCB4#,
            16#6345A0DC5FBBD519#, 16#8628FE269B9465CA#,
            16#1E5D01603F9C51EC#, 16#4DE44006A15049B7#,
            16#BF6C70E5F776CBB1#, 16#411218F2EF552BED#,
            16#CB0C0708705A36A3#, 16#E74D14754F986044#,
            16#CD56D9430EA8280E#, 16#C12591D7535F5065#,
            16#C83223F1720AEF96#, 16#C3A0396F7363A51F#
         )
      );

   ---------------------------------------------------------------------
   -- Type definitions
   ---------------------------------------------------------------------

   --+---[Packed_Block]-------------------------------------------------
   --|   Type for handling Tiger input blocks as an array of
   --|   64-bit values.
   --+------------------------------------------------------------------

   subtype Packed_Block is
      Eight_Bytes_Array(1 .. Tiger_Block_Bytes / 8);

   ---------------------------------------------------------------------
   -- Body subprogram specifications
   ---------------------------------------------------------------------

   --+---[Tiger round procedures]---------------------------------------

   procedure   Round_123(
                  S              : in out State_Registers;
                  X              : in     Eight_Bytes;
                  Mul            : in     Eight_Bytes);
   pragma Inline(Round_123);

   procedure   Round_231(
                  S              : in out State_Registers;
                  X              : in     Eight_Bytes;
                  Mul            : in     Eight_Bytes);
   pragma Inline(Round_231);

   procedure   Round_312(
                  S              : in out State_Registers;
                  X              : in     Eight_Bytes;
                  Mul            : in     Eight_Bytes);
   pragma Inline(Round_312);

   --+---[Transform]----------------------------------------------------
   --|   Purpose:
   --|   Transforms Tiger state based on input block.
   --|
   --|   Arguments:
   --|   Context              Access to TIGER_Context value that
   --|                        mantains the state to transform.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   procedure   Transform(
                  Context        : access TIGER_Context);

   --+---[Key_Schedule]-------------------------------------------------

   procedure   Key_Schedule(
                  X              : in out Packed_Block);

   --+---[Pack_Block]---------------------------------------------------
   --|   Purpose:
   --|   Packs an input block (Byte_Array) into an Eight_Bytes_Array
   --|   suitable for transformation.
   --|
   --|   Arguments:
   --|   Block          Block to pack.
   --|
   --|   Returned value:
   --|   Packed_Block corresponding to Block.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   function    Pack_Block(
                  B              : in     Byte_Array)
      return   Packed_Block;
   pragma Inline(Pack_Block);

   --+---[Unpack_State]-------------------------------------------------
   --|   Purpose:
   --|   Unpacks the state registers rendering a byte array that is the
   --|   computed digest.
   --|
   --|   Arguments:
   --|   S              State registers.
   --|
   --|   Returned value:
   --|   Unpacked Byte_Array corresponding to S.
   --|
   --|   Exceptions:
   --|   None.
   --+------------------------------------------------------------------

   function    Unpack_State(
                  S              : in     State_Registers)
      return   Byte_Array;
   pragma Inline(Unpack_State);

   ---------------------------------------------------------------------
   -- Body subprogram bodies
   ---------------------------------------------------------------------

   --+---[Round_123]----------------------------------------------------

   procedure   Round_123(
                  S              : in out State_Registers;
                  X              : in     Eight_Bytes;
                  Mul            : in     Eight_Bytes)
   is
      BA             : Byte_Array(1 .. 8);
   begin
      S(3)  := S(3) xor X;
      BA := To_Byte_Array(S(3), Big_Endian);

      S(1) := S(1) - (S_Boxes(1, BA(1)) xor
                      S_Boxes(2, BA(3)) xor
                      S_Boxes(3, BA(5)) xor
                      S_Boxes(4, BA(7)));
      S(2) := S(2) + (S_Boxes(4, BA(2)) xor
                      S_Boxes(3, BA(4)) xor
                      S_Boxes(2, BA(6)) xor
                      S_Boxes(1, BA(8)));
      S(2) := S(2) * Mul;
   end Round_123;

   --+---[Round_231]----------------------------------------------------

   procedure   Round_231(
                  S              : in out State_Registers;
                  X              : in     Eight_Bytes;
                  Mul            : in     Eight_Bytes)
   is
      BA             : Byte_Array(1 .. 8);
   begin
      S(1)  := S(1) xor X;
      BA := To_Byte_Array(S(1), Big_Endian);

      S(2) := S(2) - (S_Boxes(1, BA(1)) xor
                      S_Boxes(2, BA(3)) xor
                      S_Boxes(3, BA(5)) xor
                      S_Boxes(4, BA(7)));
      S(3) := S(3) + (S_Boxes(4, BA(2)) xor
                      S_Boxes(3, BA(4)) xor
                      S_Boxes(2, BA(6)) xor
                      S_Boxes(1, BA(8)));
      S(3) := S(3) * Mul;
   end Round_231;

   --+---[Round_312]----------------------------------------------------

   procedure   Round_312(
                  S              : in out State_Registers;
                  X              : in     Eight_Bytes;
                  Mul            : in     Eight_Bytes)
   is
      BA             : Byte_Array(1 .. 8);
   begin
      S(2)  := S(2) xor X;
      BA := To_Byte_Array(S(2), Big_Endian);

      S(3) := S(3) - (S_Boxes(1, BA(1)) xor
                      S_Boxes(2, BA(3)) xor
                      S_Boxes(3, BA(5)) xor
                      S_Boxes(4, BA(7)));
      S(1) := S(1) + (S_Boxes(4, BA(2)) xor
                      S_Boxes(3, BA(4)) xor
                      S_Boxes(2, BA(6)) xor
                      S_Boxes(1, BA(8)));
      S(1) := S(1) * Mul;
   end Round_312;

   --+---[Transform]----------------------------------------------------

   procedure   Transform(
                  Context        : access TIGER_Context)
   is
      X              : Packed_Block := Pack_Block(Context.all.Block);
      T              : State_Registers := Context.all.State;
      Tmp            : Eight_Bytes;
   begin
      Round_123(Context.all.State, X(1), 5);
      Round_231(Context.all.State, X(2), 5);
      Round_312(Context.all.State, X(3), 5);
      Round_123(Context.all.State, X(4), 5);
      Round_231(Context.all.State, X(5), 5);
      Round_312(Context.all.State, X(6), 5);
      Round_123(Context.all.State, X(7), 5);
      Round_231(Context.all.State, X(8), 5);

      Key_Schedule(X);

      Round_312(Context.all.State, X(1), 7);
      Round_123(Context.all.State, X(2), 7);
      Round_231(Context.all.State, X(3), 7);
      Round_312(Context.all.State, X(4), 7);
      Round_123(Context.all.State, X(5), 7);
      Round_231(Context.all.State, X(6), 7);
      Round_312(Context.all.State, X(7), 7);
      Round_123(Context.all.State, X(8), 7);

      Key_Schedule(X);

      Round_231(Context.all.State, X(1), 9);
      Round_312(Context.all.State, X(2), 9);
      Round_123(Context.all.State, X(3), 9);
      Round_231(Context.all.State, X(4), 9);
      Round_312(Context.all.State, X(5), 9);
      Round_123(Context.all.State, X(6), 9);
      Round_231(Context.all.State, X(7), 9);
      Round_312(Context.all.State, X(8), 9);

      for I in 4 .. Context.all.Passes loop
         Key_Schedule(X);

         Round_123(Context.all.State, X(1), 9);
         Round_231(Context.all.State, X(2), 9);
         Round_312(Context.all.State, X(3), 9);
         Round_123(Context.all.State, X(4), 9);
         Round_231(Context.all.State, X(5), 9);
         Round_312(Context.all.State, X(6), 9);
         Round_123(Context.all.State, X(7), 9);
         Round_231(Context.all.State, X(8), 9);

         Tmp      := Context.all.State(1);
         Context.all.State(1) := Context.all.State(3);
         Context.all.State(3) := Context.all.State(2);
         Context.all.State(2) := Tmp;
      end loop;

      Context.all.State(1) := Context.all.State(1) xor T(1);
      Context.all.State(2) := Context.all.State(2) - T(2);
      Context.all.State(3) := Context.all.State(3) + T(3);
   end Transform;

   --+---[Key_Schedule]-------------------------------------------------

   procedure   Key_Schedule(
                  X              : in out Packed_Block)
   is
   begin
      X(1)  := X(1) - (X(8) xor 16#A5A5A5A5_A5A5A5A5#);
      X(2)  := X(2) xor X(1);
      X(3)  := X(3) + X(2);
      X(4)  := X(4) - (X(3) xor Shift_Left((not X(2)), 19));
      X(5)  := X(5) xor X(4);
      X(6)  := X(6) + X(5);
      X(7)  := X(7) - (X(6) xor Shift_Right((not X(5)), 23));
      X(8)  := X(8) xor X(7);

      X(1)  := X(1) + X(8);
      X(2)  := X(2) - (X(1) xor Shift_Left((not X(8)), 19));
      X(3)  := X(3) xor X(2);
      X(4)  := X(4) + X(3);
      X(5)  := X(5) - (X(4) xor Shift_Right((not X(3)), 23));
      X(6)  := X(6) xor X(5);
      X(7)  := X(7) + X(6);
      X(8)  := X(8) - (X(7) xor 16#01234567_89ABCDEF#);
   end Key_Schedule;

   --+---[Pack_Block]---------------------------------------------------

   function    Pack_Block(
                  B              : in     Byte_Array)
      return   Packed_Block
   is
      R              : Packed_Block := (others => 0);
      J              : Positive := B'First;
   begin
      for I in R'Range loop
         R(I) := Make_Eight_Bytes(
                     B(J),
                     B(J + 1),
                     B(J + 2),
                     B(J + 3),
                     B(J + 4),
                     B(J + 5),
                     B(J + 6),
                     B(J + 7));
         J := J + 8;
      end loop;

      return R;
   end Pack_Block;

   --+---[Swap_Registers]-----------------------------------------------

   procedure   Swap_Registers(
                  S              : in out State_Registers)
   is
      T              : Eight_Bytes := S(1);
   begin
      S(1) := S(3);
      S(3) := S(2);
      S(2) := T;
   end Swap_Registers;

   --+---[Unpack_State]-------------------------------------------------

   function    Unpack_State(
                  S              : in     State_Registers)
      return   Byte_Array
   is
      R              : Byte_Array(1 .. TIGER_Digest_Bytes)
                           := (others => 0);
      J              : Positive := R'First;
   begin
      for I in S'Range loop
         R(J .. J + 7) := To_Byte_Array(S(I), Little_Endian);
         J := J + 8;
      end loop;

      return R;
   end Unpack_State;

   ---------------------------------------------------------------------
   -- Specification declared subprogram bodies
   ---------------------------------------------------------------------

   --+---[Allocate_Context]---------------------------------------------

   function    Allocate_Context
      return   TIGER_Context_Ptr
   is
   begin
      return new TIGER_Context;
   exception
      when others =>
         raise ACF_Storage_Error;
   end Allocate_Context;

   --+---[Deallocate_Context]-------------------------------------------

   procedure   Deallocate_Context(
                  Context     : in out TIGER_Context_Ptr)
   is
   begin
      if Context /= null then
         Free(Context);
      end if;
   end Deallocate_Context;

   --+---[Hash_Start]---------------------------------------------------

   procedure   Hash_Start(
                  Context        : access TIGER_Context)
   is
   begin
      Context.all.Passes      := Min_Passes;
      Context.all.Bit_Count   := 0;
      Context.all.State       := Initial_State;
      Context.all.Block       := (others => 0);
   end Hash_Start;

   --+---[Hash_Start]---------------------------------------------------

   procedure   Hash_Start(
                  Context        : access TIGER_Context;
                  Passes         : in     Positive)
   is
   begin
      if Passes < Min_Passes then
         Context.all.Passes   := Min_Passes;
      else
         Context.all.Passes   := Passes;
      end if;

      Context.all.Bit_Count   := 0;
      Context.all.State       := Initial_State;
      Context.all.Block       := (others => 0);
   end Hash_Start;

   --+---[Hash_Update]--------------------------------------------------

   procedure   Hash_Update(
                  Context        : access TIGER_Context;
                  Bytes          : in     Byte_Array)
   is
      I              : Positive;
      L              : Natural := Bytes'Length;
      R              : Natural;
      J              : Positive;
   begin

      --|   If input is not empty process it.

      if L > 0 then

         --|   Compute start index of context block.

         I := 1 +
              Natural(Shift_Right(Context.all.Bit_Count, 3) mod
                      Eight_Bytes(TIGER_Block_Bytes));

         --|   Increment bit count.

         Context.all.Bit_Count :=
            Context.all.Bit_Count + Shift_Left(Eight_Bytes(L), 3);

         --|   Compute the number of free slots in context block.

         R := 1 + TIGER_Block_Bytes - I;

         J := Bytes'First;

         --|   If the input length is greater than or equal to the
         --|   number of free slots perform the needed
         --|   transformations of input.

         if L >= R then

            --|   Fill context block and transform.

            Context.all.Block(I .. TIGER_Block_Bytes) :=
               Bytes(J .. J + R - 1);
            Transform(Context);

            --|   Update counters.

            J := J + R;
            L := L - R;

            --|   Transform remaining input bytes in TIGER_Block_Bytes
            --|   chunks.

            while L >= TIGER_Block_Bytes loop
               Context.all.Block :=
                  Bytes(J .. J + TIGER_Block_Bytes - 1);
               Transform(Context);
               J := J + TIGER_Block_Bytes;
               L := L - TIGER_Block_Bytes;
            end loop;

            I := 1;
         end if;

         --|   Fill context block with remaining bytes.

         while J <= Bytes'Last loop
            Context.all.Block(I) := Bytes(J);
            I := I + 1;
            J := J + 1;
         end loop;
      end if;
   end Hash_Update;

   --+---[Hash_End]-----------------------------------------------------

   function    Hash_End(
                  Context        : access TIGER_Context)
      return   Message_Digest
   is
      R              : Message_Digest;
      BC             : Byte_Array(1 .. 8);
      I              : Positive;
      BC_Offset      : Positive := 1 + TIGER_Block_Bytes - 8;
   begin

      --|   Save bit counter.

      BC := To_Byte_Array(Context.all.Bit_Count, Big_Endian);

     --|   Compute start index of context block.

      I := 1 +
           Natural(Shift_Right(Context.all.Bit_Count, 3) mod
                   Eight_Bytes(TIGER_Block_Bytes));

      --|   Perform pad

      Context.Block(I) := 16#01#;
      I := I + 1;

      if I <= TIGER_Block_Bytes then
         Context.all.Block(I .. TIGER_Block_Bytes) := (others => 0);
      end if;

      if I > BC_Offset then
         Transform(Context);
         Context.all.Block := (others => 0);
      end if;

      Context.all.Block(BC_Offset .. TIGER_Block_Bytes) := BC;
      Transform(Context);

      --|   Get digest from state.

      R := To_Message_Digest(Unpack_State(Context.all.State));

      --|   Clear context.

      Context.all.Passes      := Min_Passes;
      Context.all.Bit_Count   := 0;
      Context.all.State       := (others => 0);
      Context.all.Block       := (others => 0);

      --|   Return computed digest.

      return R;
   end Hash_End;

   --+---[Initialize]---------------------------------------------------

   procedure   Initialize(
                  Object         : in out TIGER_Context)
   is
   begin
      Object.Algo_Id    := ACF.Hash.TIGER;
      Object.Passes     := Min_Passes;
      Object.Bit_Count  := 0;
      Object.State      := (others => 0);
      Object.Block      := (others => 0);
   end Initialize;

   --+---[Finalize]-----------------------------------------------------

   procedure   Finalize(
                  Object         : in out TIGER_Context)
   is
   begin
      Object.Bit_Count  := 0;
      Object.Passes     := Min_Passes;
      Object.State      := (others => 0);
      Object.Block      := (others => 0);
   end Finalize;

end ACF.Hash.Algorithms.TIGER;
