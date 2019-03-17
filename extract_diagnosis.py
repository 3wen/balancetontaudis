"""Extract expert diagnosis from arrêtés de péril."""

from glob import glob
import re

STRUCT_ARRETE = [
    "Nous, Maire de Marseille,",
    "(Vu(.*\n)*)*",
    "(Considérant(.*\n)*)*",
    "ARRETONS",
]

# header with stamp for prefecture
RE_PREF_HEADER = r"(\s*\n)*Envoyé en préfecture le.*\n(.*\n)*.*Reçu en préfecture le.*\n(.*\n)*Affiché le.*\n(.*\n)*.*ID.?:.*VDM-AR\n"

PROG_PREF_HEADER = re.compile(RE_PREF_HEADER)

# pathologies (diagnosis)
RE_PATHO_SUIV = r"pathologies[ \n]suivantes[^:]*:(?P<pathos>(.*\n)*)(?=Considérant)"

PROG_PATHO_SUIV = re.compile(RE_PATHO_SUIV)

# long page footer: "MARSEILLE-PROVENCE 2013 CAPITALE EUROPÉENNE DE LA CULTURE" (left) + address of the city hall (center) + page number (right)
RE_LONG_FOOTER = r"\nMARSEILLE-\n(.*\n)+DE LA CULTURE\n"

PROG_LONG_FOOTER = re.compile(RE_LONG_FOOTER)

# short page footer: just the address of the city hall
RE_SHORT_FOOTER = r"Ville de Marseille, 2 quai du Port - 13233 MARSEILLE CEDEX 20 [0-9]+(/[0-9]+)?\n"

PROG_SHORT_FOOTER = re.compile(RE_SHORT_FOOTER)

RE_ANNEXE = r"(\s*\n)*ANNEXE ([0-9]+|i)"

PROG_ANNEXE = re.compile(RE_ANNEXE)


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


def strip_header(fn_txt, i, page_txt):
    """Strip header"""
    if page_txt.strip() == "":
        return page_txt
    header_pos = PROG_PREF_HEADER.search(page_txt)
    if not header_pos:
        print(repr(page_txt))
        raise ValueError("Unable to find header in {} p. {}".format(fn_txt, i))
    return page_txt[header_pos.end():]



if __name__ == "__main__":
    # extract elements quoted from the expert diagnosis
    txt_files = sorted(glob("data/arretes/plain_text/*.txt"))
    for fn_txt in txt_files:
        with open(fn_txt) as f:
            pages = f.read().split("\x0c")
            # if the first page has a préfecture stamp in
            # the header, all pages should have it
            has_pref_header = PROG_PREF_HEADER.search(pages[0])
            doc_txt = []
            for i, page_txt in enumerate(pages, start=1):
                if page_txt.strip() == "":
                    continue
                if PROG_ANNEXE.search(page_txt):
                    # exclude appendices (stop at 1st appendix)
                    break
                p_txt = page_txt
                if has_pref_header:
                    p_txt = strip_header(fn_txt, i, p_txt)
                p_txt = strip_footer(fn_txt, i, p_txt)
                doc_txt.append(p_txt)
            doc_txt = '\n'.join(doc_txt)
            # locate "pathologies suivantes"
            patho_pos = PROG_PATHO_SUIV.search(doc_txt)
            if not patho_pos:
                if "ARRÊTÉ DE MAINLEVÉE DE PÉRIL GRAVE ET IMMINENT" in doc_txt:
                    # pathologies are not in mainlevée, it's fine
                    continue
                else:
                    print(repr(doc_txt))
                    raise ValueError("Unable to locate pathologies in {}".format(fn_txt))
            pathologies = patho_pos.group("pathos")
            print(fn_txt)
            print(pathologies)
