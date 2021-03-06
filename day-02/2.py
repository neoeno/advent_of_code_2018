import functools

def has_n_chars(chars, char, n):
    return chars.count(char) == n

def has_n_any_char(chars, n):
    return any(
        has_n_chars(chars, char, n)
        for char in chars
    )

def summary_of_2_and_3_chars(ids):
    reducer = lambda summary, id: (
            summary[0] + has_n_any_char(id, 2),
            summary[1] + has_n_any_char(id, 3))
    return functools.reduce(reducer, ids, (0, 0))

def checksum(ids):
    summary = summary_of_2_and_3_chars(ids)
    return summary[0] * summary[1]


print(checksum([
"mphcuiszrnjzxwkbgdzqeoyxfa",
"mihcuisgrnjzxwkbgdtqeoylia",
"mphauisvrnjgxwkbgdtqeiylfa",
"mphcuisnrnjzxwkbgdgqeoylua",
"mphcuisurnjzxwkbgdtqeoilfi",
"mkhcuisvrnjzowkbgdteeoylfa",
"mphcoicvrnjzxwksgdtqeoylfa",
"mxhcuisvrndzxwkbgdtqeeylfa",
"dphcuisijnjzxwkbgdtqeoylfa",
"mihvuisvrqjzxwkbgdtqeoylfa",
"mphcuisrrnvzxwkbgdtqeodlfa",
"mphtuisdrnjzxskbgdtqeoylfa",
"mphcutmvsnjzxwkbgdtqeoylfa",
"mphcunsvrnjzswkggdtqeoylfa",
"mphcuisvrwjzxwkbpdtqeoylfr",
"mphcujsdrnjzxwkbgdtqeovlfa",
"mpfcuisvrdjzxwkbgdtteoylfa",
"mppcuisvrpjzxwkbgdtqeoywfa",
"mphcuisvrnjzxwkbfptqroylfa",
"mphcuisvrnjzxwkbgstoeoysfa",
"mphcufsvrnjzcwkbgdeqeoylfa",
"mphcuissrnjzxwkbgdkquoylfa",
"sphcuxsvrnjzxwkbgdtqioylfa",
"mphcuiivrhjzxwkbgdtqevylfa",
"echcuisvrnjzxwkbgltqeoylfa",
"mphcuisvrljexwkbvdtqeoylfa",
"mpjcuisvrnjzxwkhidtqeoylfa",
"mphcuisvrfjzmwkbgdtqeoylfl",
"mwhcuisvrnjzxwkbgdtqeoytfm",
"mphcuisvrsjzxwkbgdaqeoylfh",
"mohcuisvrnjzxwkbgdtqtoymfa",
"maycuisvrnjzxwkbgdtqboylfa",
"pphcuisvqnjzxwkbgdtqeoylfd",
"mprcuisvrnjtxwmbgdtqeoylfa",
"mfhcuisgrnjzxckbgdtqeoylfa",
"mphiubsvrnjzxwkbgdtqeoyufa",
"dphctisvrnjzxwkbgdtqeoylfk",
"mphcuisvrnjznwksgdtqeoyzfa",
"mpwcuisvrnjziwkbgdtqaoylfa",
"mphduzsvrnjznwkbgdtqeoylfa",
"mphccisvrnjzxwebgdtqeoylqa",
"xphcuisvrnjzxwkfvdtqeoylfa",
"mphcupsvrnjzxwkbgdtfeoylpa",
"mphcuisvrtjzjwkbgdtqeoylfe",
"mpbcuisvrnjzxwkbgdmieoylfa",
"mphcuisvrnjzxwkbgjtqetylaa",
"mphcuisvrnjzxwpbgdtgdoylfa",
"ophcufsvrqjzxwkbgdtqeoylfa",
"iphcuhsvrnjzxwkbgetqeoylfa",
"mphcuisvunjzxwwbgdtqeoylqa",
"mphcpisvrnjzowkbgdtveoylfa",
"mphcuisvrnjzxhkbgdtqeotlla",
"mphcuisvrnjzxwkbodtgeoylha",
"mphcuisvrjjzxwkbwdtqtoylfa",
"mphcwisvrnjnxwkbgjtqeoylfa",
"mplcuicqrnjzxwkbgdtqeoylfa",
"mphcuisvrnjzxydbgdtqeoylfn",
"ophckisvrnjzxwkbgdtqeozlfa",
"mphcuisvrkjzxwkbgdtteoblfa",
"yphcuisvrnjcxwkbggtqeoylfa",
"mphcuisvrnazxwfbqdtqeoylfa",
"mphcuisvrmjzxwkbgdtlwoylfa",
"mphctksvrnjzxwibgdtqeoylfa",
"mphcuisprnjzxlebgdtqeoylfa",
"mphcuisnrnjzxakbgdtueoylfa",
"mphcuiavrnjoxwtbgdtqeoylfa",
"nphcuisvrnjzxwkbgdtqzoylfk",
"mphcuisrrnjmxwkbgdtqdoylfa",
"mphcuisvrujzxwkvgdtqehylfa",
"mphcuisvrnfzxwkogdtqebylfa",
"mphcuisvrnjwdwkbgdtqeoyxfa",
"mphcuisvrntzxwkrgxtqeoylfa",
"mpzcuisvrnjzxwebgdtqeoylsa",
"aphcuikvrnjzxwwbgdtqeoylfa",
"mphcqisvrnjzxwkpgdtqeoelfa",
"mphcuusvrnjzxwkbgdtjeodlfa",
"mphcuisvrnjzewkbgdtteoylza",
"mphcuisvanjzxwkbgdtheoylfc",
"mphcjishrnjzxwkbgltqeoylfa",
"mpxcuislrnjzxwkbgdtqeoynfa",
"mphcuisvrnjjxwkbgdtmeoxlfa",
"mphcimsvrnjzxwkbsdtqeoylfa",
"mphcxisvcnjzxwjbgdtqeoylfa",
"mphcuisbrvjzxwkbgdtqeoymfa",
"mplcuisvrnjzxwkbgdtaenylfa",
"mphcuihvrnjzxwkygytqeoylfa",
"mphcbisvrnjzxhkbgdtqezylfa",
"mphcuisarnjzxwkbgatqeoylfv",
"mphcumsvrnjzxwkbgdrqebylfa",
"mlhcuisvrnwzxwkbgdtqeoylfx",
"mpkcuisvrkjzxwkbgdtqeoylfo",
"mphcuissrnjzxwkbgdtqmoylfc",
"mphcuiwvrnjuxwkfgdtqeoylfa",
"mphcuicvlnjzxwkbgdvqeoylfa",
"mphcuisvrvvzxwkbfdtqeoylfa",
"myhcuisvrnjpxwkbgntqeoylfa",
"mpocuisvrnjzxwtbgitqeoylfa",
"mphcuisvrnjzxwkbgdtwewyqfa",
"mphcuisvtnjzxwwbgdtqeoolfa",
"mphcuisvrnjzxgkbgdyqeoyyfa",
"mphcuisvrdjzxwkbgpyqeoylfa",
"bphcuisvrnjzxwkbgxtqefylfa",
"sphcuisvrdjzxwktgdtqeoylfa",
"mphcuvsvrnjmxwobgdtqeoylfa",
"mphcuisvrnjzxwkbsdtqeuylfb",
"mnhcmisvynjzxwkbgdtqeoylfa",
"mphckisvrnjzxwkhgdkqeoylfa",
"mpacuisvrnjzxwkbgdtqeoolaa",
"mpgcuisvrnjzxwkbzdtqeoynfa",
"mphcuisvrojzxwkbzdtqeoylga",
"mphcuisvknjfxwkbydtqeoylfa",
"mphcuistrnjzxwkbgdqqeuylfa",
"bpvcuiszrnjzxwkbgdtqeoylfa",
"mphcuxsvrnjzswkbgdtqeoelfa",
"mphcuisvbnjzxwlbgdtqeoylla",
"mphcuisvonczxwkbgktqeoylfa",
"mphcuisvrnkzxwvbgdtquoylfa",
"mphcuisvrnjzxokfgdtqeoylia",
"tphcuisvrnjzxwkbjdwqeoylfa",
"mihcuisvrnjzpwibgdtqeoylfa",
"mphcuisvrejzxwkbgdtqjuylfa",
"mprcuisvrnjixwkxgdtqeoylfa",
"mpqcuiszrnjzxwkbgdtqeodlfa",
"mphcuasvrnjzzakbgdtqeoylva",
"mphcuisvrnjzmwkbtdtqeoycfa",
"mphcuisvrnjzxwkbcdtqioylxa",
"mphckisvrnjzxwkbcdtqeoylfm",
"mphcuisvrnjuxwbogdtqeoylfa",
"mphcuisdrnjzxwkbldtqeoylfx",
"mphcuisvrnjoxwkbgdtqeyyyfa",
"mphcuicvqnjzxwkbgdtqeoylna",
"mpmcuisvrnjzxwkbgdtqktylfa",
"mphcuisvrnqzxwkggdtqeoykfa",
"mphcuisvryjzxwkbydtqejylfa",
"mphcugsvrnjzxwkbghtqeeylfa",
"rphcuusvrnjzxwkwgdtqeoylfa",
"zphwuiyvrnjzxwkbgdtqeoylfa",
"cphcuivvrnjzxwkbgdtqenylfa",
"mphcuisvrnjzxwkagotqevylfa",
"mprcuisvrcjzxwkbgdtqeoytfa",
"mphjugsvrnezxwkbgdtqeoylfa",
"mphcuisvryjzxwkbgltqeoylaa",
"mphcursvrnjzxfkbgdtqeoydfa",
"mphcuisvrcuzxwkbgdtqeoylfw",
"mphcuisvrijzxwkbgdtqeoelfh",
"xphcuisvenjzxjkbgdtqeoylfa",
"mphcuisvrnazxwkbgdeqeoylaa",
"mphcuisbrsjzxwkbgdtqeoygfa",
"mlhvuisvrnjzxwkbgdtqeoylfh",
"mphcuisvrnjzxukbgdtqeoyhfy",
"mpzcuilvrnjzawkbgdtqeoylfa",
"hphcuisjfnjzxwkbgdtqeoylfa",
"mahcuisvrnjzxwkegdtqeoylfi",
"mphcuixvrnjzcwkbgdtqetylfa",
"mphcuisvrnjzxwkdgdtqeoklfj",
"mlhcuisvrnjzxwkbgdteeoylka",
"mphcuifvrnjbxwkrgdtqeoylfa",
"mphcuasvrnjzzwkbgdtqeoylva",
"mphcuisvrnjzxwkboutqeoylba",
"mbhcuisvcnjzxwklgdtqeoylfa",
"mpbcuisvrnjzxgkbgdtqesylfa",
"mphcuisvrnjfswkbgdtqeoylfd",
"mphcuisvrnjzxwkbgdoweoysfa",
"uphcuisvrnjzrwkbgdtqelylfa",
"mphcuisvrnjzxwkbgdtqyoylsi",
"mpqcuiqvxnjzxwkbgdtqeoylfa",
"mphcuisorfjzxwkbgatqeoylfa",
"mphcuisvrntfxwkbzdtqeoylfa",
"mphcuisvrnrzxwkbgdtueoylfl",
"mphcuisvrnjzewkagdtyeoylfa",
"mpocuisdrnjzxwkbgdtqeozlfa",
"mphcuisvrnjjxwkbgdtoeoylfm",
"mphcuisvenjzxwkbgdtqwoylza",
"mpmcuisvrnjzxwkbgdtqeoxlfr",
"mphcuisvgnjhxwkbgdtqeoplfa",
"mphcuisvrnjzowkdgdtqeoyyfa",
"mphcuisqynjzxwkbgdtqeoylda",
"hphcuisvgnjzxwkbgdtbeoylfa",
"iphcuipvrnuzxwkbgdtqeoylfa",
"mphcuisvrnjzsikbpdtqeoylfa",
"mpwcuhsvrnjzxbkbgdtqeoylfa",
"mnhjuisvcnjzxwkbgdtqeoylfa",
"mphcudsvrnjzxwkbgdtqloilfa",
"mpncuiwvrwjzxwkbgdtqeoylfa",
"mphcuisvrnjgawkbgdtqeoylya",
"mphcuisvrnjzxwkbggtteoslfa",
"mphcuisvrnjzxwkbgdvqeoylpe",
"mphcuisvrnczxfkbgktqeoylfa",
"mphcuifvrnjzxwkbgdbmeoylfa",
"mphcuisvrnjytwkbgdtqeoylla",
"mphcuisvrnjzxwkbgdtjeoxlfn",
"mphjuisvrnjzxwkbghtqeoyffa",
"mphcuisvrnjzxkrbgdtqeoylaa",
"mphcbisvrnjzxwkbgttqeoylfs",
"mphkuksvbnjzxwkbgdtqeoylfa",
"nphcuidvrnjzxwhbgdtqeoylfa",
"mphguzsvrnjzxwkbgdaqeoylfa",
"mihcuisfrnjzxwkbgdtqhoylfa",
"mphcuisvrnrzxwpbgdtqesylfa",
"zphcuisvrnjzxwkbddtqeoylaa",
"mphcuigvmnjzxwkbgdtqeoylba",
"mjhcuisvrnjzxjkbgdtqeoylha",
"mphnuisvrnjznwkbgdtqnoylfa",
"mkhcuisvrnjcxwkbgdqqeoylfa",
"mphcuisvenjzxwbbqdtqeoylfa",
"qphcuisnrnjzawkbgdtqeoylfa",
"mphcuisvrdjzxwkbgdtqeoywca",
"mphcuzsvvnjzxwfbgdtqeoylfa",
"pphcuxsvrnjzxwkbgdtmeoylfa",
"mphiuvsvrnjzxlkbgdtqeoylfa",
"mphlqisvrnjzxkkbgdtqeoylfa",
"mmhcuisvrnjzxwkbgatqeoylea",
"mphduisrrnjoxwkbgdtqeoylfa",
"mphcuisvrnjnxwkvgdyqeoylfa",
"mphcuvsvrnjzxgkbgdtqeoylfz",
"mphcuisvryjzxwkbggtqkoylfa",
"iphcuisvrdjzxwkbgotqeoylfa",
"mphcuisvrnjzxwhbgdtqwoyofa",
"mphcorbvrnjzxwkbgdtqeoylfa",
"mghcuisvrnpzxykbgdtqeoylfa",
"mphauisvrnjnxwkbzdtqeoylfa",
"mphcgisvrnjzxwkwgdtqeoygfa",
"mphcuisvrnjzxwkggotqeoylba",
"mphcuesvrnjzxwkbgdwqebylfa",
"yphcuisvrnjzxwkbgdxqeoylja",
"ephyuisvrnjzywkbgdtqeoylfa",
"mfhcuisqrnjzxwkbgdlqeoylfa",
"mphkuisvrnjzxwkbertqeoylfa",
"mphcuusgrnjzxwkbggtqeoylfa",
"mphcuildrnjvxwkbgdtqeoylfa",
"mphcuiuvrnjzlwkbgwtqeoylfa",
"mppcuisvrljzxwkbgdtqeoylfw",
"mphcwiwvrnjzxwsbgdtqeoylfa",
"mphcubivrnjzxwkqgdtqeoylfa",
"mphcuisvrnjpxwkngdtqeoylpa",
"pchcuisvrgjzxwkbgdtqeoylfa",
"mphcuisvlnjzxwkbgdtmeoylfw",
"mphcuisvrnjzywkbgdvqeoylfj",
"mpzcuisvrnezxwktgdtqeoylfa",
"mphcuisvrnjbxwkbgzrqeoylfa",
"mphcuisvrnjzxwktgdtqeodtfa",
"jphcuiavrnjzxwkbgdtqeoylfv",
"mphcuisvrnjzxwkbddppeoylfa",
"mphcuissrkjzxwkbgxtqeoylfa",
"mphcuisvrhjzxwxbgdtqeoylxa",
"mphcvisvgnjjxwkbgdtqeoylfa",
"mphcuisprnjwxwtbgdtqeoylfa",
"mphcuissrnjzxqkbgdtqeoymfa",
"mphcuiabrnjzxokbgdtqeoylfa",
"mphcuisvrnczxwkbgmtpeoylfa"
]))
