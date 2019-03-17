"""Extract expert diagnosis from arrêtés de péril.

TODO:
- [ ] extract "préconisations" (a few occurrences are currently erroneously captured as pathologies),

"""

from glob import glob
import os
import re
import warnings

import pandas as pd


# docs where the OCR is empty or worthless
ERR_OCR = [
    "data/arretes/plain_text/40-rue-daubagne-13001_2019_00202.pdf.txt",
]

ML_TYPES = [
    "MAINLEVÉE PARTIELLE D'ARRÊTÉ DE PÉRIL GRAVE ET IMMINENT",
    "MAINLEVÉE PARTIELLE DE PÉRIL IMMINENT",
    "MAIN LEVÉE D'ARRÊTÉ DE PÉRIL GRAVE ET IMMINENT",
    "MAIN LEVEE D'ARRÉTÉ DE PÉRIL GRAVE ET IMMINENT",
    "MAIN-LEVÉE D'ARRÊTÉ DE PÉRIL GRAVE ET IMMINENT",
    "ARRÊTÉ DE MAINLEVÉE PARTIELLE DE PÉRIL GRAVE ET IMMINENT",
    "ARRÊTE DE MAIN LEVEE PARTIELLE DE PERIL GRAVE ET IMMINENT",
    "ARRETE DE MAINLEVEE PARTIELLE DE PERIL GRAVE ET IMMINENT",
    "ARRÊTÉ DE MAINLEVÉE DE PÉRIL GRAVE ET IMMINENT",
    "ARRÊTÉ DE MAIN LEVÉE DE PÉRIL GRAVE ET IMMINENT",
    "ARRÊTÉ DE MAINLEVÉE DE PÉRIL IMMINENT",
    "ARRÊTE DE MAINLEVÉE DE PÉRIL IMMINENT",
    "ARRÊTÉ DE MAINLEVÉE PARTIELLE DE PÉRIL IMMINENT",
    "ARRÊTÉ MAINLEVÉE DE PÉRIL IMMINENT",
    "ARRETE MAINLEVEE DE PERIL IMMINENT",
    "ARRETE MAINLEVEE PERIL IMMINENT",
    "ARRÊTÉ DE RÉINTÉGRATION PARTIELLE",
    "ARRÊTÉ DE RÉINTÉGRATION",
]

OTHER_TYPES = [
    "ARRÊTÉ DE DÉCONSTRUCTION",
    "ARRÊTÉ MODIFICATIF DE DÉCONSTRUCTION",
    "ARRETE MODIFICATIF DE PERIL GRAVE ET IMMINENT",
    "ARRETE D'EVACUATION",
    "ARRÊTÉ MODIFICATIF",
]


# output when 2 columns are detected ; the left col is printed
# entirely before the right column and that changes the whole
# processing
RE_2_COLS = r"^(Article \d+\n\s*\n)+MARSEILLE-\nPROVENCE 2013\n.*CAPITALE\n[\n]?.*EUROPÉENNE\nDE LA CULTURE\n"

PROG_2_COLS = re.compile(RE_2_COLS)

# header with stamp for prefecture
# note: ID sometimes ocr'd as 1D
RE_PREF_HEADER = r"(\s*\n)*Envoyé en préfecture le.*\n(.*\n)*.*Reçu en préfecture le.*\n(.*\n)*Affiché le.*\n(\s*\n)*(.*(1D|ID).?:.*VDM-AR\n)?"

PROG_PREF_HEADER = re.compile(RE_PREF_HEADER)

# pathologies (diagnosis)
RE_PATHO_SUIV = r"(Considérant (que )?le rapport d’expertise susvisé[^:]*|pathologies[ \n]suivantes|pathologie[ \n]suivante|désordres[ \n]constructifs[ \n]suivant[s]?|nombre[ \n]important de pathologies|constatant le mauvais état)[^:]*:(\s*\n)*(?P<pathos>(.*\n)*?)(?=Considérant)"

PROG_PATHO_SUIV = re.compile(RE_PATHO_SUIV)

# short alternative formulation
RE_PATHO_SHORT = r"Considérant que (l'expert|le rapport d’expertise susvisé,) reconnaît l’état.*\n.*et constate (?P<pathos>(.*\n)*?)(?=Considérant)"

PROG_PATHO_SHORT = re.compile(RE_PATHO_SHORT)

# long page footer: "MARSEILLE-PROVENCE 2013 CAPITALE EUROPÉENNE DE LA CULTURE" (left) + address of the city hall (center) + page number (right)
RE_LONG_FOOTER = r"\nMARSEILLE-\n(.*\n)+DE LA CULTURE\n"

PROG_LONG_FOOTER = re.compile(RE_LONG_FOOTER)

# short page footer: just the address of the city hall
RE_SHORT_FOOTER = r"Ville de Marseille, 2 quai du Port - 13233 MARSEILLE CEDEX 20 [0-9]+(/[0-9]+)?\n"

PROG_SHORT_FOOTER = re.compile(RE_SHORT_FOOTER)

# mandatory last article: "Le présent arrêté peut faire l'objet de recours devant le Tribunal Administratif\ndans un délai de 2 mois à compter de sa notification." ;
# absent in at least one occurrence, hence not mandatory?
RE_RECOURS_TA = r"dans un délai de 2 mois à compter de sa notification."

PROG_RECOURS_TA = re.compile(RE_RECOURS_TA)

# signature
RE_SIGNATURE = r"\nSigné le :"

PROG_SIGNATURE = re.compile(RE_SIGNATURE)


# appendices
RE_ANNEXE = r"((?<!en )|(?<! : ))ANNEXE ([0-9]+|i)"

PROG_ANNEXE = re.compile(RE_ANNEXE)


def strip_left_col(fn_txt, i, page_txt):
    """Strip left column (2 col layout)"""
    if page_txt.strip() == "":
        return page_txt
    left_col_pos = PROG_2_COLS.search(page_txt)
    if not left_col_pos:
        print(repr(page_txt))
        raise ValueError("Unable to find left column in {} p. {}".format(fn_txt, i))
    return page_txt[left_col_pos.end():]


def strip_header(fn_txt, i, page_txt):
    """Strip header"""
    if page_txt.strip() == "":
        return page_txt
    header_pos = PROG_PREF_HEADER.search(page_txt)
    if not header_pos:
        print(repr(page_txt))
        raise ValueError("Unable to find header in {} p. {}".format(fn_txt, i))
    return page_txt[header_pos.end():]


def strip_footer(fn_txt, i, page_txt):
    """Strip footer"""
    if page_txt.strip() == "":
        return page_txt
    # look for long footer
    long_footer_pos = PROG_LONG_FOOTER.search(page_txt)
    if not long_footer_pos:
        short_footer_pos = PROG_SHORT_FOOTER.search(page_txt)
        if not short_footer_pos:
            print(repr(page_txt))
            raise ValueError("Unable to find footer in {} p. {}".format(fn_txt, i))
        return page_txt[:short_footer_pos.start()]
    return page_txt[:long_footer_pos.start()]


if __name__ == "__main__":
    all_files = []
    all_pathos = []
    all_comments = []
    # extract elements quoted from the expert diagnosis
    txt_files = sorted(glob("data/arretes/plain_text/*.txt"))
    for fn_txt in txt_files:
        bn_pdf = os.path.splitext(os.path.basename(fn_txt))[0]
        all_files.append(bn_pdf)
        if fn_txt in ERR_OCR:
            # skip files where OCR output is empty or worthless
            all_pathos.append('')
            all_comments.append('bad OCR')
            continue
        with open(fn_txt) as f:
            # each formfeed marks a page break
            pages = f.read().split("\x0c")
            # "mainlevée" does not contain pathologies
            if any(x in pages[0] for x in ML_TYPES):
                # pathologies are not in mainlevée, it's fine
                all_pathos.append('')
                all_comments.append('mainlevée')
                continue
            elif any(x in pages[0] for x in OTHER_TYPES):
                # other types of docs
                all_pathos.append('')
                all_comments.append('autre')
                continue
            # if the first page has a préfecture stamp in
            # the header, all pages should have it
            has_pref_header = PROG_PREF_HEADER.search(pages[0])
            # gather document text page by page, cleaning
            # header and footer, until the last mandatory
            # article (délai de recours TA)
            doc_txt = []
            for i, page_txt in enumerate(pages, start=1):
                p_txt = page_txt
                if p_txt.strip() == "":
                    # skip empty page
                    # print('Empty page {}'.format(i))
                    continue
                elif PROG_2_COLS.search(p_txt):
                    # strip the left column to avoid prematurely
                    # starting to capture footer
                    p_txt = strip_left_col(fn_txt, i, p_txt)
                #
                if has_pref_header:
                    p_txt = strip_header(fn_txt, i, p_txt)
                p_txt = strip_footer(fn_txt, i, p_txt)
                doc_txt.append(p_txt)
                # break at the end of the main doc (excluding
                # appendices)
                if PROG_RECOURS_TA.search(p_txt):
                    # if this page contains the mandatory last
                    # article (délai de recours TA)
                    break
                elif PROG_SIGNATURE.search(p_txt):
                    # or if it contains the signature
                    break
            # assemble doc text
            doc_txt = '\n'.join(doc_txt)
            # locate and retrieve the list of pathologies
            patho_pos = list(PROG_PATHO_SUIV.finditer(doc_txt))
            if not patho_pos:
                # alternative, short formulation
                patho_pos = list(PROG_PATHO_SHORT.finditer(doc_txt))
            if not patho_pos:
                all_pathos.append('?')
                all_comments.append('?')
                warnings.warn("Unable to locate pathologies in {}".format(fn_txt))
                continue
            print(fn_txt)  # verbose
            pathologies = [m.group("pathos") for m in patho_pos]
            pathologies = '\n'.join(pathologies)
            # print(pathologies)
            # print('-----')
            # minor cleanup
            pathologies = re.sub("PROVENCE 2013", "", pathologies)
            pathologies = re.sub("CAPITALE", "", pathologies)
            pathologies = re.sub("(\s*\n){3,}", "\n\n", pathologies)
            # print(pathologies)
            all_pathos.append(pathologies)
            all_comments.append('')
    print(len([x for x in all_comments if x == '']), "/", len(txt_files))
    print(len(all_files), len(all_pathos), len(all_comments))
    # make into a DataFrame for quick vis
    df_pathos = pd.DataFrame(data={'fname': all_files, 'pathologies': all_pathos, 'comments': all_comments})
    df_pathos = df_pathos[df_pathos['pathologies'] != '']
    with open('pathos.csv', mode='w') as f_out:
        df_pathos.to_csv(f_out, index=False)
