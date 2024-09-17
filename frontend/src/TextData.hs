module TextData (textData) where

import Data.Text (Text)

textData :: Text
textData = "init:\nようこそ。\n君が來るのをを 待ってゐたよ。\nさあ この世界を 見て。\n;a_block.Anna.1_md_textA0 a_block.Tana.1_md_textT0 sm_0 cd_w sc_1 md_scene0\n\nmap0:\n*******************\n**************s****\n***A***************\n*******************\n*******************\n*******************\n********Y@*********\n****************C**\n*******************\n******B************\n*******************\n*******************\n*************g*****\n*******************\n\nobj0:\nA,Anna,live,,block,north\nB,Box,block,,move,north\nC,Conbiner,func tool tool tool,map1,enter,north\nY,Yoko,live,,block,north\ng,Glasses,tool,,get,nodir\ns,Super,tool,,get,nodir\nG,SuperG,tool,,get,nodir\nm,Mask,tool,,get,nodir\nV,virus1,move 5,,block,north\nV,virus2,move 4,,block,north\nV,virus3,attack 4,,block,north\na,ame,tool,,get,nodir\nP,PCR,func tool tool,map3,enter,nodir\n\nmap1:\n**********@\n***********\n*Cgs*=*G***\n***********\n******T****\n**********%\n\nobj1:\nT,Tana,live,,block,north\ng,Glasses,tool,,block,nodir\ns,Super,tool,,block,nodir\nG,SuperG,tool,,block,nodir\nC,Conbiner,tool,,block,nodir\n\nmap2:\n**********@\n***********\n*C*m*=*P***\n***********\n***********\n**********%\n\nobj2:\nC,Conbiner,tool,,block,nodir\nm,Mask,tool,,block,nodir\nP,PCR,tool,,block,nodir\n\nmap3:\n**********@\n***********\n*P***=*X***\n***********\n*****T*****\n**********%\n\nobj3:\nT,Tana,live,,block,north\nP,PCR,tool,,block,nodir\n\nscene0:\n君が今までゐた世界とは ずいぶん違って見えるかな。\n・・・ともかく 今 君の力が必要なんだ。\nどうしてなのか 何をすればいいのか 正直どう話せばいいか 分からない。\nそれより 少し この世界を見てまはってくれないか。\nたぶん それが一番いいと思ふんだ。\n;a_block.Yoko.0_if_?pHave.Super_md.textYs_if_?pHave.Glasses_md.textYg_if_?pHave.SuperG_md.textYG_if_?pHave.Mask_md.textYM0_el_md.textY0 a_pushto.Box-Yoko.0_md_textYB a_attack.Box.3_co_Box-Mask a_block.Super.0_md_texts0 a_block.Glasses.0_md_textg0 a_block.SuperG.0_md_textG0 a_block.Conbiner.0_md_textC0\n\ntexts0:\nスーパー\n\ntextg0:\nメガネ\n\ntextG0:\nスーパーメガネ\n\ntextC0:\n結合器\n\ntextm0:\nマスク\n\ntextP0:\nPCR\n\n\ntextY0:\n;sc_1\nどうか このあたりを 見てまはってくれないか。\n何か氣になるものがあったら 教へてほしい。\n\ntextY1:\n;sc_1\n結合器？。\n・・・さうか・・・。\nその結合器に 材料を並べて 反對側から たたいたら どうなるかな・・・。\n\ntextYs:\n;sc_1\nこれは 何だらうね？。\nスーパー？。\nよく分からないけど 何かといっしょに使ふのかな。\n\ntextYg:\n;sc_1\nメガネだね。\nそんなものが どうしてあるんだらう。\n\ntextYB:\n;sc_1\nなんの箱だらう これは？。\nたたいたら こはせるのかな？。\n\ntextYM0:\n;sc_1\nマスク？。\n結構きめが細かいっていふ 不織布のマスクだね。\nでも この世界で必要なのかな？。\n\ntextYG:\n;sc_1\nスーパーメガネ？。\nメガネの何が スーパーになるっていふんだらう？。\nあっ！ メガネが消えた！。\n;cn p_virus1.7.4_virus2.14.9_virus3.4.8 a_attack.virus1.0_md_textV10 a_block.virus1.1_md_textV11 a_attack.virus2.0_md_textV20 a_block.virus2.1_md_textV21 a_attack.virus3.1_md_textV30 a_block.virus3.0_md_textV31\nなんだ あれは？。\n;d_block.Yoko a_block.Yoko.0_if_?pHave.Mask_md.textYM1_if_?counter.v1,1,v2,1,v3,1_md.textY3_el_md.textY2 ac_yoko\n\ntextY2:\n;sc_1\nこれなんだ。\nきっと これが 君の力なんだよ！。\n今 確實に この世界に變化が來てゐる。\nあいつらが 何なのか 僕には分からないんだけれど。\nどうか この世界を守る手掛かりを つかんで欲しいんだ。\n\ntextY3:\n;sc_1\n・・・さうか・・・ ウイルスたちが そんなことを・・・。\nたしか 1976年に流行ったってされてゐる 新型豚インフルエンザ も たくさんの嘘の宣傳が流されて 打つ必要のなかった有害なワクチンが アメリカ国民に大量投与されたんだよね。\n今では 色んな事實が 明らかになってゐるけど その當事の人たちは 一部を除いて テレビが流す情報をすっかり 鵜呑みにしてしまった・・・。\nでも假に 僕たちが コントロールされてゐるとしたって たぶん 科學的に どんなに正しい理論で人を説得しても 世の中の流れを變へるのは難しいんだらうな・・・。\n力を持つ人は より強力な言葉で 他の人に影響を及ぼしていくから。\n假に マスクや ワクチンが 「感染」に對して 何ら効果がなく むしろ害のあるものだ といふことを自分自身で理解したとしても・・・。\n;d_block.Yoko a_block.Yoko.0_md_textY4 ud_Conbiner.map2 ac_yoko2 a_block.Mask.0_md_textm0 a_block.PCR.0_md_textP0 d_block.Tana a_block.Tana.1_md_textT4\n\ntextY4:\n;sc_1\n長老なら 何か良い方法を知ってゐるかも・・・。\n\ntextYM1:\n;sc_1\nマスクだ。\nこれを装着すれば ウイルス感染のリスクが減るって言はれてゐるけど・・・。\n\ntextV10:\nいたた！。\nちょっと 話を聞いてくれよ！。\n\ntextV20:\nうわ！。やめて〜！。\nいたいよ〜！。\n\ntextV11:\nぼくらは 通常 ウイルスって呼ばれてる。\nもともとの ラテン語では 病氣をおこす毒の素 みたいな意味だし ぼくたちが嫌はれるのも 分かる。\n實際に ぼくらの働きによって病氣が引き起こされることもあるからね。\nでも 少なくとも この世界に歪みをもたらしてゐるのは ぼくらぢゃないんだ。\n昔から いつもあるものを 君たちは 自然 って言ってるよね。\nさういふ意味では ぼくらだって 自然の一部なんだ。\n世界を歪めてるのは ぼくら ではなくて ある種の 「情報」を廣めてゐる 一部の人間なんだよ。\n;ac_v1\n\ntextV21:\n君たちの 歴史をふり返って ちょっと考えてほしいんだ。\n世の中の多くの人たちが 「正しい」って思ってゐたことが 實は 少數の人たちによって ただ思ひこまされてゐただけだっだ ってことが 今までたくさんあったよね？。\nそれは 少數の人たちが 多くの人たちを コントロールするために 必要だと考へられたんだ。\n今君が「正しい」と思ってることは どんな實驗をして どんなふうに証明されたのか ちゃんと分かってゐるかい？。\nもし さうでないのなら もしかしたら君だって ただ「支配」されるために その知識を「正しい」と思わされてゐるのかもしれないよ。\n;ac_v2\n\ntextV30:\nお！。なんか 目が冴えてきたよ。\nさうさう 君に傳へておきたいことがあるんだ。\nある言葉が どんな意味として 定義されてゐるのか。\nそれを確認しないで ただイメージだけで 言葉を使ったり 聞いたりしてゐると いつのまにか まともな人を傷つけたり 自分自身を どんどん追いこんでいったりするものなんだ。\n例えば 「感染」といふ言葉が どう定義されてゐるか 君は分かって その言葉を使ってゐるかい？。\nウイルスが「感染」した といふのは どういふ状態をいふのか。\nそれを証明するには 何が分かればよいのか。\n「専門家」に任せるんぢゃなくて まず 自分自身で言葉の意味を 確認して欲しい。\n歴史上 「専門家」が 間違ったことを言ふのは あまりにも ありふれてゐるのに 今現在その可能性を考へない といふのは ちょっと おかしいと思ふんだ。\n;ac_v3 d_block.virus3\n\ntextV31:\nふわ〜。\nなんか ねむたくて 言ひたいこと 忘れちゃったよ〜。\nちょっと 喝を入れてくれたら 思ひだすかも〜。\n\ntextA0:\n;sc_2\nあなたは どこからきたの？。\n私は道に迷ってしまったの。\nここは どこなんだらうね。\nこの世界は なんだか 色も形も 少ないから・・・\nでも わたし ここだから迷ってゐる といふわけでもないかもしれない。\nあっちの世界に居たときから 迷ってゐたんだと思ふ。\n自分が どこに居て 何をすればいいのか・・・。\n;a_block.Anna.1_md_textA1\n\ntextA1:\n;sc_2\nここでは お腹も減らないし 喉も渇かない。\nでもそれが 私には怖い。\n前の世界では 死んでしまふことが怖かった。\nここでは 變はらないことが 怖い。\nけれど あなたなら 何かを變へてくれる。\nそんな氣がするの。\nだって かうやって わたしに 話しかけてくれたんだもの・・・。\n;a_block.Anna.0_if_?counter.yoko,1_md.textA3_el_md.textA2\n\ntextA2:\n;sc_2\nなにか おもしろいことがあったら 教へてね。\nここには あまり おもしろいことが ない・・・いいえ。\nわたしが おもしろいことを 探さうとしてなかった。\nあなたが來るまでは・・・。\n\ntextA3:\n;sc_2\nキャー！ なに この ゲジゲジした虫は！。\n氣持ち惡いよ〜！。\nでも なんだらう・・・。\n今 氣持ち惡い って感じてゐる自分が 少し心地いいの・・・。\nおかしいよね 私・・・。\n;d_block.Anna a_block.Anna.0_if_?counter.yoko2,1_md.textA5_el_md.textA4\n\ntextA4:\n;sc_2\nずっと 變はらなかった この世界・・・。\nまるで 滅びを待つかのやうに。\nいいえ たぶん ずっと變はらない といふことが 消滅する っていふことなのかも。\nけれど 今は違ふ。\nあなたのおかげ。\nすべてが めまぐるしく 變はっていくの・・・。\n\ntextA5:\n;sc_2\n長老様は 氣まぐれに見へるけれど。\nなにか とても 深いところを ご覧になってゐるやうな氣がするの。\nただ 今までは それが 變はらない世界の ありさまの一つだった・・・。\nあなたが 長老様と會って 何かを知れば きっと 何かが變はると思ふ・・・。\nそれから わたし あなたと話してゐて 思ひ出したことがあるの。\n見て！。\nこれは フルーツ味の あめちゃん。\nかわいいでしょ？。\nわたし こんなものを 持ってゐたこと すっかり忘れてゐたの。\nただ たぶん この世界では 食べても 味はしないし もともと お腹も減らないから 私が持ってゐても あまり役にたたないと思ふ。\nでも あなたが 何かに使ってくれるなら とても嬉しいし ワクワクする！。\nもらってくれるかな？。\n;cho_はい_textA8_いいえ_textA6\n\ntextA6:\n;sc_2\nさう・・・。\nでも もし必要になったら 声をかけてね。\n;d_block.Anna a_block.Anna.0_md_textA7\n\ntextA7:\n;sc_2\nあめちゃん いる？。\n;cho_はい_textA8_いいえ_textA6\n\ntextA8:\n;sc_2 if_?pHave.any_md.textA10_el_md.textA9\n\ntextA9:\n;sc_2\nはい どうぞ。\n;gt_ame d_block.Anna a_block.Anna.0_md_textA11\n\ntextA10:\n;sc_2\nいちど 持ってゐるものを どこかに置いてから また來てくれる？。\n\ntextA11:\n;sc_2\nあめちゃん まうひとつ 私持ってるの。\nそれもいる？。\n;cho_はい_textA13_いいえ_textA12\n\ntextA12:\n;sc_2\nもし必要になったら 声をかけてね。\n;d_block.Anna a_block.Anna.0_md_textA11\n\ntextA13:\n;sc_2 if_?pHave.any_md.textA10_el_md.textA14\n\ntextA14:\n;sc_2\nはい どうぞ。\n;gt_ame d_block.Anna a_block.Anna.0_md_textA15\n\ntextA15:\n;sc_2\nつぎは どんなことが起こるのかしら。\nとても樂しみ！。\n\ntextT0:\n;sc_4\nここがどこか 知りたいか？。\n;cho_はい_textT1_いいえ_textT2\n\ntextT1:\n;sc_4\nここは おぬしが入ってきた 結合器の中ぢゃ。\nなにかと なにかを くっつけて なにかをつくる・・・。\nその 決まりを この部屋で決めておるのぢゃ。\n;a_block.Tana.0_md_textT3 a_block.Yoko.1_md_textY1\n\ntextT2:\n;sc_4\nさうか ならば 何も言ふまい・・・。\n\ntextT3:\n;sc_4\nわしに 言へることは それだけぢゃ・・・。\n\ntextT4:\n;sc_4\nおぬしにとって 「現實」とは 何ぢゃ？。\nもし それが おぬしの目の前にあるものだとしたら 今 見てゐるその画面も おぬしにとって 「現實」といふことぢゃな？。\nそれとも 何か 別の 全人類が共有するやうな 絶對的な「現實」が 存在するといふのかの？。\n・・・フォッフォッフォ！。なんぢゃ その キツネにつままれたやうな 顔は？。\n・・・「現實」とは おぬしが 「現實」と感じてゐるもの であって それ以外の何ものでもないのぢゃ。\nもし それ以外に何か 別の「現實」があるとして おぬしは その存在を証明できるか？。\n決して できまいて。\n二千五百年以上前に 仏陀は それを「唯識」として教え 現代では 認知科學が それを脳の「内部表現」として扱っておる。\n要するに おぬしの心で見てゐる世界が この世界のすべて すなはち 「現實」といふことぢゃ。\n;a_block.Tana.1_md_textT5\n\ntextT5:\n;sc_4\nこの認識は 現代の量子力學の「多世界」解釈と相俟って 各人がそれぞれ 別々の宇宙を 無數に創造し 各々違った「現實」の中を生きてゐる といふ世界観へと導く。\nそれでは 何故 別々の宇宙を生きてゐる筈の 我々が 同じ空間を共有してゐると感じ 互ひに 心を通はせ合ふ 共感し合ふといふことが起こるのか？。\nそれは 我々が ある種の共振状態におかれるからぢゃ。\n素粒子ひとつの存在が 波動函數で表現されることを知っておるか？。\nすなはち 我々を構成する原子の さらに細かい構成要素は 波として記述できる。\nその 素粒子によって出来てゐる 我々の體も その物理實體の原因ともなる 我々の意識 心も ある特定の周波數で 波打っておる。\n別の振動數の波同士といふものは 時間が経つと その振動数を互ひに合はせるやうに 振動するやうになる。\nこれが共振現象ぢゃ。\n我々の心と心にも 無意識のうちに これが起きておる。\nホメオスタシス同調とも呼ばれる この共振現象 ・・・。\nこれが 同じ空間 同じ「現實」 同じ気持ちを我々が共有するやうに感じる からくりぢゃ。\nだが實際 我々一人一人は 全く異なる宇宙を體驗しており それは 我々一人一人の 心が生み出しておるのぢゃ。\n;a_block.Tana.1_md_textT6\n\ntextT6:\n;sc_4\nそろそろ この 長たらしい説教めいた話にも 飽きてきたぢゃろう・・・。\nなぜ わしが 長々と こんな話をしたのか。\nそれは 世界といふものは おぬしの心ひとつで 實は どうにでもなる といふことを傳へたかったのぢゃ。\nそんなことはない とおぬしは 強く反發するぢゃらう。\n長い年月をかけて おぬしは あるひとつの世界観を信じるやう 生きてきたからぢゃ。\n「世界はひとつ」 といふ 世界観を。\nわしの話は それを ひっくり返すやうなものぢゃろ？。\nだが もし假に 今はまだ よく理解できなかったとしても おぬしがこの世界を本氣で變へたいと 思ふならば ここは 理解する必要があるのぢゃ。\nそして 世界をコントロールしやうと本氣で考へ 實行してゐる連中は ここをよく理解しておる。\n鍵は おぬしら ひとりひとりの心だ といふことを。\nおぬしら ひとりひとりの 信じたことが そのまま次の世界を創る。\nだからこそ おぬしらに ある種の「情報」を信じこませることによって おぬしらを 自分たちのコントロール下に置いておけるのぢゃ。\nそのやうな行為が積み重なり 世界の特定の部分に 金と権力が集中し おぬしのやうな民は 氣づかぬうちに搾取され 日々奴隷化しておる。\nおぬしが さう思はないのは さう思はなくなる程 麻痺させられておるからぢゃ。\nおぬしの中に やらなければならない事と やりたい事は どの程度あるか 考へてみなさい。\nもし やらなければならない事が やりたい事より 圧倒的に多いなら おぬしは 完全に奴隷状態 といふことぢゃ。\nおぬしの仕へておる ご主人様は おぬしの目にとまらぬところに ゐる もしくは ある。だから 自分が奴隷などとは 思ひもしないのぢゃよ。\n人々の自由が奪はれていったあげくの果てには 世界の均衡そのものが崩れ 當の支配者は シェルターを造ったり 他の惑星への移住を本氣で考へたりしておる始末ぢゃ。\n莫大な金と権力を持つ支配者自身が いつ世界が崩壊してもおかしくないと 現在の地球の永続をあきらめ とにかく自分達と関係者だけは 良い環境で生き延びやうと その莫大な財を投資しておる。 \n彼らは 世界が創られる仕組みを 理解して利用してはゐるが 實のところ 信じてゐない いや どうしても 信じたくないのぢゃよ。\nそこが彼らの 唯一の弱みであり 本當の意味で彼らが 決っして望みを達成できない理由なのぢゃ。\n今こそ おぬしら ひとりひとりの持つ 心の力と その意味を とり戻さねばならぬ・・・。\n・・・むむ・・・何か 外が騒がしいぞ・・・？。\n\n\n\n"