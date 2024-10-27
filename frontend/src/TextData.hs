module TextData (textData) where

import Data.Text (Text)

textData :: Text
textData = "init:\nなぜ $r人-ひと は $r夢-ゆめ を$r見-み るのだらう。\n夢と $r現實-げんじつ は とても $r違-ちが ふやうにみへる。\nけれども $r私達-わたしたち は 夢を $r經驗-けいけん する。\n$r人生-じんせい が $r絶-た へ$r間-ま ない 經驗の$r積-つ み$r重-かさ ねならば。\n夢も その人生の$r一部-いちぶ なのだ。\n;sm_0 md_level0\n\nemerald:\nこれは $r偽-いつは りのない $r真實-しんじつ である。\n上なるものは 下なるが$r如-ごと く。\n下なるものは 上なるが如く。\nこれは ひとへに $r一-いち なるものの $r奇跡-きせき を行ふためである。\n$r萬物-ばんぶつ は 一より 生ずる。\n一により すべてのものが $r媒介-ばいかい され $r適應-てきおう により $r創造-さうざう される。\n\n太陽は その父であり 月は その母であり。\n風は それを$r胎内-たいない で運び 地は その$r乳母-うば である。\nこれは 全世界の すべての完成の父であり 地に$r至-いた りて$r統合-とうがふ される。\n\n地を 火から。 $r粗大-そだい なものから $r微細-びさい なものを $r勤-つと めて$r丁寧-ていねい に $r分離-ぶんり せよ。\nそれは 地から天に$r昇-のぼ り $r再-ふたた び地へと降りて 上なるものと 下なるものの力を受けとる。\nこの方法により $r汝-なんぢ は 全世界の$r榮光-えいくわう を$r得-え  すべてのものが 明らかになる。\n\nその力は すべての力を$r凌-しの ぎ $r精妙-せいめう なものにも$r勝-まさ り あらゆる$r堅固-けんこ なものを$r貫通-かんつう する。\nかうして 世界は 創造された。\nこのやうにして すべての$r素晴-すば らしい適應が 行なはれたのである。\n\nそれ$r故-ゆへ  私は 世界の$r総合-そうがふ 的な$r哲學-てつがく の 三つの$r要素-えうそ を持つ $r三重-みへ に$r偉大-ゐだい なヘルメス と呼ばれる。\n以上で 私は 太陽の作業について語るべきことを$r完了-くわんれう した。\n\n    「エメラルド・タブレット」。\n\ntest0:\n二$r種類-しゅるい の人間がゐる。\n世界の$r真實-しんじつ を探す人間。\nそんなことは どうでもいい人間。\n$r後者-こうしゃ は 人生のどこかで $r探求-たんきゅう を止めてしまった。\n$r物心-ものごころ つく前に 止めた者。\n$r學-まな びつかれて 止めた者。\nその$r原因-げんいん を 知る者と 知らぬ者。\nそれぞれが この世界の$r綾-あや をなす。\nだが その$r意識-いしき は 探すつもりがなくとも。\nその$r無意識-むいしき は 探し$r續-つづ けてゐる。\n$r己-おのれ の$r意味-いみ を。\n世界の真實を。\n\nmap0:\n・・・・・・・北・・\n・・・・・・・・・・\n・・・・・・・・・・\n・・・・＠・・東・・\n・・・・・・・・・・\n・・国・中・・・・・\n九・・・・・関・・・\n・・四・近・・・・・\n・・・・・・・・・・\n\nmim0:\n9\n\nobj0:\n関,Higasi,tool,mapKantou,enter,nodir,blue\n\nlevel0:\n・・・・・。\nここは 何だ？。\n・・・。\nなんとなく ボタンの←↑→↓で$r方向-はうかう を$r變-か へたり $r移動-いどう したりできる$r氣-き がする・・・。\n;a_block.Kanasaki.1_md_textKana0\n;a_block.Tochigi.0_md_textTochi0\n;a_block.Gunma.0_md_textGun0\n\ntextSave:\n;save\nセーブしました\n\ntextBack:\nセーブをやめました\n\nmapKantou:\n・・・・・・・・・・・＠\n・・・・・・・・・・・・\n・・・・・栃・・・・・・\n・・群・・・・茨・・・・\n・・・・・・・・・・か・\n・・・・埼・・・・・・・\n・・・・・東・・千・・・\n・・・神・・・・・・・・\n・・・・・・・・・・・・\n・・・・・・・・・・・※\n\nmimKantou:\n10\n\nobjKantou:\nか,Kanasaki,live,,block,north,blue\n栃,Tochigi,block,,block,noDir,blue\n群,Gunma,block,,block,nodir,blue\n\ntextTochi0:\nこの県の首都は？。\n;ip_宇都宮_textTochi1_textTochi2\n\ntextTochi1:\nせいかいです。\n;d_block.Tochigi\n;uo_Kantou.Tochigi.tool,mapTochigi,enter,nodir,orange\n\ntextTochi2:\nちがふよ。\n\ntextGun0:\nこの県の首都は？。\n;ip_前橋_textGun1\n\ntextGun1:\nせいかい！。\n\ntextKana0:\n;sc_4\nこまったのう・・・。\n;a_block.Kanasaki.1_md_textKana1\n\ntextKana1:\n;cho_どうしたのですか？_textKana2_ここはどこですか？_textKana3\n\ntextKana2:\n;sc_4\n・・・$r姫-ひめ がのう・・・。\n・・・いや・・・こちらの話ぢゃ・・・。\n\ntextKana3:\n;sc_4\nうむ？？。\nおぬしは・・・。$r誰-だれ ぢゃ？。\n\nmapTochigi:\n・・・・・・・・・＠\n・・・・・・・・・・\n・・・・・・・・・・\n・・・・・・・・・・\n・・・・・・・・・・\n・・・・・・・・・・\n・・・・・宇・・・・\n・・・・・・・・・・\n・・・・・・・・・・\n・・・・・・・・・※\n\nmimTochigi:\n11\n\nobjTochigi:\n宇,Utunomiya,block,,block,nodir,blue\n"