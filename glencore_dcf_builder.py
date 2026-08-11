"""
Glencore plc (LSE: GLEN) -- Discounted Cash Flow model builder
================================================================
Builds a 5-tab DCF workbook (Cover, Assumptions, DCF, Sensitivity,
Deal Context) valuing Glencore standalone, framed around the
Rio Tinto / Glencore cross-border takeover talks (Jan-Aug 2026) --
the case study picked from JPMorgan's 2026 Global M&A Mid-Year
Outlook.

Method: 5-year explicit unlevered FCF forecast (FY2026E-FY2030E) off
a FY2025A base (Glencore FY2025 Preliminary Results), WACC via CAPM,
Gordon Growth terminal value cross-checked against an implied exit
EV/EBITDA multiple. All hardcoded inputs are sourced/dated on the
Assumptions and DCF tabs (Notes columns) and colour-coded blue;
every other cell is a live formula.

Usage:
    python3 build.py
    python3 recalc.py glencore_dcf.xlsx    (from Anthropic's xlsx
    skill -- runs a LibreOffice recalc so cached formula values
    populate; without it, formula cells read back as empty until
    opened in Excel/Sheets, which recalculate on open anyway)

Requires: openpyxl (pip install openpyxl). Output: glencore_dcf.xlsx
in the working directory. Plain ASCII throughout, no line over 100
chars, single save at the end -- written to load cleanly anywhere.
"""
import openpyxl
from openpyxl import Workbook
from openpyxl.styles import Font, PatternFill, Alignment, Border, Side
from openpyxl.utils import get_column_letter, column_index_from_string

OUTPUT_PATH = "glencore_dcf.xlsx"

# ---------------------------------------------------------------
# Styles (per the model's own colour convention, documented on Cover)
# ---------------------------------------------------------------
BLUE = Font(name="Arial", size=10, color="0000FF")
BLUE_BOLD = Font(name="Arial", size=10, color="0000FF", bold=True)
BLACK = Font(name="Arial", size=10, color="000000")
BLACK_BOLD = Font(name="Arial", size=10, color="000000", bold=True)
GREEN = Font(name="Arial", size=10, color="008000")
GREEN_BOLD = Font(name="Arial", size=10, color="008000", bold=True)
NOTE_FONT = Font(name="Arial", size=8, italic=True, color="808080")
TITLE_FONT = Font(name="Arial", size=15, bold=True, color="1F3864")
SUBTITLE_FONT = Font(name="Arial", size=10, italic=True, color="404040")
LABEL_FONT = Font(name="Arial", size=10, color="000000")
LABEL_BOLD = Font(name="Arial", size=10, bold=True, color="000000")
SECTION_FONT = Font(name="Arial", size=11, bold=True, color="FFFFFF")
SECTION_FILL = PatternFill("solid", fgColor="1F3864")
COLHDR_FONT = Font(name="Arial", size=10, bold=True, color="FFFFFF")
COLHDR_FILL = PatternFill("solid", fgColor="2E5395")
YELLOW_FILL = PatternFill("solid", fgColor="FFFF9C")

THIN = Side(style="thin", color="BFBFBF")
BORDER_ALL = Border(left=THIN, right=THIN, top=THIN, bottom=THIN)
TOP_BORDER = Border(top=Side(style="thin", color="000000"))

FMT_USD_MM = '$#,##0;($#,##0);"-"'
FMT_USD_PS = '$#,##0.00;($#,##0.00);"-"'
FMT_PCT = '0.0%;(0.0%);"-"'
FMT_MULT = '0.00"x"'
FMT_NUM0 = '#,##0;(#,##0);"-"'
FMT_BETA = '0.00'
FMT_GBP_P = '#,##0.0'


def sect(ws, row, text, last_col_letter="I"):
    """Full-width dark-blue section header bar."""
    end = column_index_from_string(last_col_letter)
    for col in range(1, end + 1):
        ws.cell(row=row, column=col).fill = SECTION_FILL
    c = ws.cell(row=row, column=1, value=text)
    c.font = SECTION_FONT
    ws.row_dimensions[row].height = 18


def lbl(ws, row, text, bold=False, col=1):
    """Row label, normally in column A."""
    c = ws.cell(row=row, column=col, value=text)
    c.font = LABEL_BOLD if bold else LABEL_FONT
    return c


def note(ws, row, text, col):
    """Small grey source/assumption note."""
    c = ws.cell(row=row, column=col, value=text)
    c.font = NOTE_FONT
    c.alignment = Alignment(wrap_text=True, vertical="top")
    return c


def inp(ws, row, col, value, fmt=None, bold=False):
    """Hardcoded input / assumption cell -- blue."""
    c = ws.cell(row=row, column=col, value=value)
    c.font = BLUE_BOLD if bold else BLUE
    if fmt:
        c.number_format = fmt
    return c


def frm(ws, row, col, formula, fmt=None, bold=False):
    """Formula cell -- black."""
    c = ws.cell(row=row, column=col, value=formula)
    c.font = BLACK_BOLD if bold else BLACK
    if fmt:
        c.number_format = fmt
    return c


def link(ws, row, col, formula, fmt=None, bold=False):
    """Cross-sheet link -- green."""
    c = ws.cell(row=row, column=col, value=formula)
    c.font = GREEN_BOLD if bold else GREEN
    if fmt:
        c.number_format = fmt
    return c


def build():
    wb = Workbook()
    ws_cover = wb.active
    ws_cover.title = "Cover"
    ws_a = wb.create_sheet("Assumptions")
    ws_d = wb.create_sheet("DCF")
    ws_s = wb.create_sheet("Sensitivity")
    ws_m = wb.create_sheet("Deal Context")

    build_assumptions(ws_a)
    build_dcf(ws_d)
    build_sensitivity(ws_s)
    build_deal_context(ws_m)
    build_cover(ws_cover)
    polish(wb, ws_cover, ws_a, ws_d, ws_s, ws_m)

    wb.save(OUTPUT_PATH)
    print("Saved " + OUTPUT_PATH + " -- run recalc.py next "
          "to populate cached formula values.")


def build_assumptions(ws):
    ws.column_dimensions['A'].width = 40
    ws.column_dimensions['B'].width = 14
    ws.column_dimensions['C'].width = 62

    ws.cell(row=1, column=1,
            value="GLENCORE PLC (LSE: GLEN) -- DCF ASSUMPTIONS"
            ).font = TITLE_FONT
    ws.cell(row=2, column=1,
            value="All figures in USD unless stated. Blue = input "
                  "/ assumption. Black = formula."
            ).font = SUBTITLE_FONT

    sect(ws, 4, "MARKET DATA  (as of 7 Aug 2026)", "C")

    lbl(ws, 5, "Share price (GBp)")
    inp(ws, 5, 2, 557.00, FMT_GBP_P)
    note(ws, 5, "Yahoo Finance UK, GLEN.L, close 7 Aug 2026.", 3)

    lbl(ws, 6, "GBP/USD FX rate")
    inp(ws, 6, 2, 1.35, FMT_BETA)
    note(ws, 6, "Spot GBP/USD, Aug 2026 (XE / Yahoo Finance).", 3)

    lbl(ws, 7, "Share price ($)")
    frm(ws, 7, 2, "=B5/100*B6", FMT_USD_PS)

    lbl(ws, 8, "Diluted shares outstanding (mm)")
    inp(ws, 8, 2, 11719, FMT_NUM0)
    note(ws, 8, "~11.7bn per multiple data providers; cross-checked "
                "against quoted market cap divided by share price.", 3)

    lbl(ws, 9, "Market capitalisation ($mm)")
    frm(ws, 9, 2, "=B7*B8", FMT_USD_MM)

    lbl(ws, 10, "Net debt, YE2025A ($mm)")
    inp(ws, 10, 2, 11200, FMT_USD_MM)
    note(ws, 10, "Glencore FY2025 Preliminary Results (glencore.com, "
                 "18 Feb 2026); incl. $1.0bn marketing lease "
                 "liabilities. Net debt/EBITDA 0.83x.", 3)

    lbl(ws, 11, "Enterprise Value, current implied ($mm)", bold=True)
    frm(ws, 11, 2, "=B9+B10", FMT_USD_MM, bold=True)

    sect(ws, 13, "COST OF EQUITY (CAPM)", "C")

    lbl(ws, 14, "Risk-free rate (US 10Y Treasury)")
    inp(ws, 14, 2, 0.046, FMT_PCT)
    note(ws, 14, "US 10-year Treasury yield, 10 Aug 2026 "
                 "(Reuters / TradingEconomics).", 3)

    lbl(ws, 15, "Beta (peer-referenced estimate)")
    inp(ws, 15, 2, 0.85, FMT_BETA)
    note(ws, 15, "Analyst judgment. Sourced 5Y-monthly regression "
                 "betas for GLEN disperse widely (Yahoo Finance: "
                 "0.51; MarketBeat: 1.21) -- too unstable to use "
                 "directly. 0.85 is referenced off closest "
                 "diversified-miner peer BHP Group (beta 0.84, "
                 "MarketBeat, Aug 2026), consistent with Glencore's "
                 "Marketing segment providing some earnings "
                 "stabilisation vs. a pure-play miner. See "
                 "Sensitivity tab for a full WACC 6-12% range.", 3)

    lbl(ws, 16, "Equity risk premium")
    inp(ws, 16, 2, 0.050, FMT_PCT)
    note(ws, 16, "Standard long-run market ERP assumption.", 3)

    lbl(ws, 17, "Cost of equity", bold=True)
    frm(ws, 17, 2, "=B14+B15*B16", FMT_PCT, bold=True)

    sect(ws, 19, "COST OF DEBT", "C")

    lbl(ws, 20, "Pre-tax cost of debt (estimate)")
    inp(ws, 20, 2, 0.058, FMT_PCT)
    note(ws, 20, "Estimate: ~120bp spread over 10Y Treasury, "
                 "consistent with A3 (Moody's) / BBB+ (S&P) ratings "
                 "(Glencore H1 2025 Report, 6 Aug 2025).", 3)

    lbl(ws, 21, "Normalised effective tax rate (estimate)")
    inp(ws, 21, 2, 0.28, FMT_PCT)
    note(ws, 21, "FY2025A reported a net tax credit (H1: -$278mm) "
                 "on one-off items -- not representative of a "
                 "run-rate rate; 28% used as a normalised "
                 "through-cycle estimate.", 3)

    lbl(ws, 22, "After-tax cost of debt", bold=True)
    frm(ws, 22, 2, "=B20*(1-B21)", FMT_PCT, bold=True)

    sect(ws, 24, "WACC", "C")

    lbl(ws, 25, "Weight of equity  E/(D+E)")
    frm(ws, 25, 2, "=B9/(B9+B10)", FMT_PCT)

    lbl(ws, 26, "Weight of debt  D/(D+E)")
    frm(ws, 26, 2, "=B10/(B9+B10)", FMT_PCT)

    lbl(ws, 27, "WACC", bold=True)
    frm(ws, 27, 2, "=B25*B17+B26*B22", FMT_PCT, bold=True)
    ws.cell(row=27, column=2).fill = YELLOW_FILL

    sect(ws, 29, "GROWTH & TERMINAL ASSUMPTIONS", "C")

    lbl(ws, 30, "Terminal growth rate")
    inp(ws, 30, 2, 0.025, FMT_PCT)
    note(ws, 30, "Long-run nominal assumption, broadly in line "
                 "with global GDP + inflation trend.", 3)

    lbl(ws, 31, "Adjusted EBITDA growth -- FY26E")
    inp(ws, 31, 2, 0.07, FMT_PCT)
    lbl(ws, 32, "Adjusted EBITDA growth -- FY27E")
    inp(ws, 32, 2, 0.06, FMT_PCT)
    lbl(ws, 33, "Adjusted EBITDA growth -- FY28E")
    inp(ws, 33, 2, 0.045, FMT_PCT)
    lbl(ws, 34, "Adjusted EBITDA growth -- FY29E")
    inp(ws, 34, 2, 0.035, FMT_PCT)
    lbl(ws, 35, "Adjusted EBITDA growth -- FY30E")
    inp(ws, 35, 2, 0.03, FMT_PCT)
    note(ws, 31, "Analyst estimate: copper-volume ramp (FY25A "
                 "production 951.6kt; management guides material "
                 "further growth this decade) plus a ~$1bn "
                 "identified cost-savings programme, fading to a "
                 "normalised rate. Single blended rate -- not a "
                 "commodity-by-commodity price/volume build; see "
                 "Cover tab limitations.", 3)

    lbl(ws, 36, "D&A growth rate p.a.")
    inp(ws, 36, 2, 0.035, FMT_PCT)
    note(ws, 36, "Assumed to track the growing capital/asset base; "
                 "deliberately decoupled from EBITDA's "
                 "commodity-price cyclicality.", 3)

    lbl(ws, 37, "NWC increase (% of EBITDA change)")
    inp(ws, 37, 2, 0.05, FMT_PCT)
    note(ws, 37, "Stylised proxy for incremental Industrial-segment "
                 "working capital. Glencore's large Marketing-"
                 "segment working capital (readily marketable "
                 "inventories) is separately trade-financed and "
                 "excluded.", 3)

    sect(ws, 39, "CAPEX SCHEDULE ($mm)", "C")

    lbl(ws, 40, "Capex -- FY26E")
    inp(ws, 40, 2, 7200, FMT_USD_MM)
    lbl(ws, 41, "Capex -- FY27E")
    inp(ws, 41, 2, 7500, FMT_USD_MM)
    lbl(ws, 42, "Capex -- FY28E")
    inp(ws, 42, 2, 7800, FMT_USD_MM)
    lbl(ws, 43, "Capex -- FY29E")
    inp(ws, 43, 2, 7500, FMT_USD_MM)
    lbl(ws, 44, "Capex -- FY30E")
    inp(ws, 44, 2, 7300, FMT_USD_MM)
    note(ws, 40, "Analyst estimate: FY25A net capex $6.9bn "
                 "(Glencore FY2025 Prelim Results) stepped up for "
                 "copper growth capex, moderating after 2028 as "
                 "major projects complete.", 3)

    tall_rows = (15, 20, 21, 31, 36, 37, 40)
    for r in [5, 6, 8, 10, 14, 15, 16, 20, 21, 30, 31, 32, 33, 34,
              35, 36, 37, 40, 41, 42, 43, 44]:
        ws.row_dimensions[r].height = 26 if r in tall_rows else 14


def build_dcf(ws):
    ws.column_dimensions['A'].width = 34
    for col in "BCDEFG":
        ws.column_dimensions[col].width = 12
    ws.column_dimensions['H'].width = 3
    ws.column_dimensions['I'].width = 55

    ws.cell(row=1, column=1,
            value="GLENCORE -- UNLEVERED FREE CASH FLOW  "
                  "($ in millions unless noted)").font = TITLE_FONT
    ws.cell(row=2, column=1,
            value="Explicit forecast FY2026E-FY2030E off a "
                  "FY2025A base; Gordon Growth terminal value."
            ).font = SUBTITLE_FONT

    years = ["FY2025A", "FY2026E", "FY2027E", "FY2028E", "FY2029E",
             "FY2030E"]
    for i, y in enumerate(years):
        col = 2 + i
        cell = ws.cell(row=4, column=col, value=y)
        cell.font = COLHDR_FONT
        cell.fill = COLHDR_FILL
        cell.alignment = Alignment(horizontal="center")
    ws.row_dimensions[4].height = 16

    lbl(ws, 5, "Adjusted EBITDA", bold=True)
    inp(ws, 5, 2, 13511, FMT_USD_MM, bold=True)
    frm(ws, 5, 3, "=B5*(1+Assumptions!$B$31)", FMT_USD_MM, bold=True)
    frm(ws, 5, 4, "=C5*(1+Assumptions!$B$32)", FMT_USD_MM, bold=True)
    frm(ws, 5, 5, "=D5*(1+Assumptions!$B$33)", FMT_USD_MM, bold=True)
    frm(ws, 5, 6, "=E5*(1+Assumptions!$B$34)", FMT_USD_MM, bold=True)
    frm(ws, 5, 7, "=F5*(1+Assumptions!$B$35)", FMT_USD_MM, bold=True)
    note(ws, 5, "FY25A: Adjusted EBITDA $13,511mm, Glencore FY2025 "
                "Preliminary Results (glencore.com, 18 Feb 2026). "
                "FY26E-30E grown per Assumptions tab.", 9)

    lbl(ws, 6, "  % growth y/y")
    for i in range(1, 6):
        col = 2 + i
        prev = get_column_letter(col - 1)
        cur = get_column_letter(col)
        c = ws.cell(row=6, column=col, value="=" + cur + "5/" +
                    prev + "5-1")
        c.font = NOTE_FONT
        c.number_format = FMT_PCT

    lbl(ws, 7, "D&A")
    inp(ws, 7, 2, 7533, FMT_USD_MM)
    frm(ws, 7, 3, "=B7*(1+Assumptions!$B$36)", FMT_USD_MM)
    frm(ws, 7, 4, "=C7*(1+Assumptions!$B$36)", FMT_USD_MM)
    frm(ws, 7, 5, "=D7*(1+Assumptions!$B$36)", FMT_USD_MM)
    frm(ws, 7, 6, "=E7*(1+Assumptions!$B$36)", FMT_USD_MM)
    frm(ws, 7, 7, "=F7*(1+Assumptions!$B$36)", FMT_USD_MM)
    note(ws, 7, "FY25A = Adjusted EBITDA less Adjusted EBIT "
                "($13,511mm minus $5,978mm), both per Glencore "
                "FY2025 Prelim Results. Grown at assumed rate "
                "thereafter.", 9)

    lbl(ws, 8, "EBIT")
    for col in range(2, 8):
        L = get_column_letter(col)
        frm(ws, 8, col, "=" + L + "5-" + L + "7", FMT_USD_MM)

    lbl(ws, 9, "Tax")
    for col in range(3, 8):
        L = get_column_letter(col)
        frm(ws, 9, col, "=-" + L + "8*Assumptions!$B$21", FMT_USD_MM)
    note(ws, 9, "Normalised tax rate per Assumptions tab. Not "
                "computed for FY25A -- see note on that year's "
                "reported tax credit.", 9)

    lbl(ws, 10, "NOPAT")
    for col in range(3, 8):
        L = get_column_letter(col)
        frm(ws, 10, col, "=" + L + "8+" + L + "9", FMT_USD_MM)

    lbl(ws, 11, "Add: D&A")
    for col in range(3, 8):
        L = get_column_letter(col)
        frm(ws, 11, col, "=" + L + "7", FMT_USD_MM)

    lbl(ws, 12, "Less: Capex")
    capex_rows = {3: 40, 4: 41, 5: 42, 6: 43, 7: 44}
    for col, arow in capex_rows.items():
        frm(ws, 12, col, "=-Assumptions!$B$" + str(arow), FMT_USD_MM)

    lbl(ws, 13, "Less: Increase in NWC")
    for col in range(3, 8):
        L = get_column_letter(col)
        P = get_column_letter(col - 1)
        formula = "=-(" + L + "5-" + P + "5)*Assumptions!$B$37"
        frm(ws, 13, col, formula, FMT_USD_MM)

    lbl(ws, 14, "Unlevered Free Cash Flow", bold=True)
    for col in range(3, 8):
        L = get_column_letter(col)
        c = frm(ws, 14, col, "=SUM(" + L + "10:" + L + "13)",
                 FMT_USD_MM, bold=True)
        c.border = TOP_BORDER

    lbl(ws, 16, "Discount period")
    for i, col in enumerate(range(3, 8), start=1):
        c = ws.cell(row=16, column=col, value=i)
        c.font = BLACK
        c.number_format = FMT_NUM0

    lbl(ws, 17, "Discount factor")
    for col in range(3, 8):
        L = get_column_letter(col)
        formula = "=1/(1+Assumptions!$B$27)^" + L + "16"
        frm(ws, 17, col, formula, "0.000")

    lbl(ws, 18, "PV of Unlevered FCF", bold=True)
    for col in range(3, 8):
        L = get_column_letter(col)
        frm(ws, 18, col, "=" + L + "14*" + L + "17", FMT_USD_MM,
            bold=True)

    sect(ws, 20, "TERMINAL VALUE", "C")

    lbl(ws, 21, "Terminal year FCF (FY2031E)")
    frm(ws, 21, 3, "=G14*(1+Assumptions!$B$30)", FMT_USD_MM)

    lbl(ws, 22, "Terminal Value (Gordon Growth)")
    frm(ws, 22, 3,
        "=C21/(Assumptions!$B$27-Assumptions!$B$30)", FMT_USD_MM)

    lbl(ws, 23, "PV of Terminal Value")
    frm(ws, 23, 3, "=C22*G17", FMT_USD_MM)

    lbl(ws, 24, "Memo: implied exit EV/EBITDA multiple")
    frm(ws, 24, 3, "=C22/G5", FMT_MULT)
    note(ws, 24, "Cross-check only; primary terminal method is "
                 "Gordon Growth via WACC and terminal growth "
                 "(see Assumptions).", 9)

    sect(ws, 26, "VALUATION BRIDGE", "C")

    lbl(ws, 27, "Sum of PV of FCF (FY26E-FY30E)")
    frm(ws, 27, 3, "=SUM(C18:G18)", FMT_USD_MM)

    lbl(ws, 28, "PV of Terminal Value")
    frm(ws, 28, 3, "=C23", FMT_USD_MM)

    lbl(ws, 29, "Enterprise Value", bold=True)
    c = frm(ws, 29, 3, "=C27+C28", FMT_USD_MM, bold=True)
    c.border = TOP_BORDER

    lbl(ws, 30, "Less: Net debt (YE2025A)")
    frm(ws, 30, 3, "=-Assumptions!$B$10", FMT_USD_MM)

    lbl(ws, 31, "Equity Value", bold=True)
    c = frm(ws, 31, 3, "=C29+C30", FMT_USD_MM, bold=True)
    c.border = TOP_BORDER

    lbl(ws, 32, "Diluted shares outstanding (mm)")
    frm(ws, 32, 3, "=Assumptions!$B$8", FMT_NUM0)

    lbl(ws, 33, "Implied Value per Share ($)")
    frm(ws, 33, 3, "=C31/C32", FMT_USD_PS)

    lbl(ws, 34, "Implied Value per Share (GBp)", bold=True)
    c = frm(ws, 34, 3, "=C33/Assumptions!$B$6*100", FMT_GBP_P,
             bold=True)
    c.fill = YELLOW_FILL

    lbl(ws, 35, "Current share price (GBp)")
    frm(ws, 35, 3, "=Assumptions!$B$5", FMT_GBP_P)

    lbl(ws, 36, "Implied upside / (downside)", bold=True)
    frm(ws, 36, 3, "=C34/C35-1", FMT_PCT, bold=True)


def build_sensitivity(ws):
    ws.column_dimensions['A'].width = 3
    ws.column_dimensions['B'].width = 16
    for col in "CDEFG":
        ws.column_dimensions[col].width = 11
    ws.column_dimensions['I'].width = 50

    ws.cell(row=1, column=1,
            value="SENSITIVITY -- Implied Value per Share (GBp)"
            ).font = TITLE_FONT
    ws.cell(row=2, column=1,
            value="Each cell independently recomputes the DCF "
                  "(fixed operating FCFs) at that row's WACC and "
                  "column's terminal growth rate."
            ).font = SUBTITLE_FONT

    wacc_vals = [0.06, 0.07, 0.08, 0.09, 0.10, 0.11, 0.12]
    g_vals = [0.015, 0.02, 0.025, 0.03, 0.035]

    hdr = ws.cell(row=4, column=2,
                  value="WACC (down) / Term. g (right)")
    hdr.font = COLHDR_FONT
    hdr.fill = COLHDR_FILL
    hdr.alignment = Alignment(horizontal="center", wrap_text=True)
    for j, g in enumerate(g_vals):
        col = 3 + j
        cell = ws.cell(row=4, column=col, value=g)
        cell.font = COLHDR_FONT
        cell.fill = COLHDR_FILL
        cell.number_format = FMT_PCT
        cell.alignment = Alignment(horizontal="center")
    ws.row_dimensions[4].height = 26

    for i, wv in enumerate(wacc_vals):
        row = 5 + i
        cell = ws.cell(row=row, column=2, value=wv)
        cell.font = COLHDR_FONT
        cell.fill = COLHDR_FILL
        cell.number_format = FMT_PCT
        for j, g in enumerate(g_vals):
            col = 3 + j
            gcol = get_column_letter(col)
            b = "$B" + str(row)
            gc = gcol + "$4"
            formula = (
                "=((DCF!$C$14/(1+" + b + ")^1"
                "+DCF!$D$14/(1+" + b + ")^2"
                "+DCF!$E$14/(1+" + b + ")^3"
                "+DCF!$F$14/(1+" + b + ")^4"
                "+DCF!$G$14/(1+" + b + ")^5)"
                "+(DCF!$G$14*(1+" + gc + ")/(" + b + "-" + gc + "))"
                "/(1+" + b + ")^5"
                "-Assumptions!$B$10)/Assumptions!$B$8"
                "/Assumptions!$B$6*100"
            )
            c = ws.cell(row=row, column=col, value=formula)
            c.font = BLACK
            c.number_format = FMT_GBP_P
            c.border = BORDER_ALL
            if abs(wv - 0.08) < 1e-9 and abs(g - 0.025) < 1e-9:
                c.fill = YELLOW_FILL

    note(ws, 5, "Yellow cell approx. base case (WACC 8% / g 2.5%; "
                "exact base case is on the Assumptions/DCF tabs "
                "and will not sit exactly on this grid).", 9)
    note(ws, 6, "Grid deliberately spans a wide WACC range given "
                "the dispersion in sourced beta estimates (see "
                "Assumptions tab, Cost of Equity section).", 9)

    lbl(ws, 13, "For reference:", bold=True)
    lbl(ws, 14, "Base-case WACC (Assumptions tab)")
    link(ws, 14, 3, "=Assumptions!$B$27", FMT_PCT)
    lbl(ws, 15, "Base-case terminal growth (Assumptions tab)")
    link(ws, 15, 3, "=Assumptions!$B$30", FMT_PCT)
    lbl(ws, 16, "Base-case implied value per share (GBp)")
    link(ws, 16, 3, "=DCF!$C$34", FMT_GBP_P, bold=True)
    lbl(ws, 17, "Current share price (GBp)")
    link(ws, 17, 3, "=Assumptions!$B$5", FMT_GBP_P)


def build_deal_context(ws):
    ws.column_dimensions['A'].width = 14
    ws.column_dimensions['B'].width = 26
    ws.column_dimensions['C'].width = 62
    ws.column_dimensions['D'].width = 34

    ws.cell(row=1, column=1,
            value="M&A CONTEXT -- Rio Tinto / Glencore, "
                  "Jan-Aug 2026").font = TITLE_FONT
    ws.cell(row=2, column=1,
            value="Cross-border, scale-driven mining consolidation "
                  "-- the theme highlighted in the JPMorgan 2026 "
                  "Global M&A Mid-Year Outlook."
            ).font = SUBTITLE_FONT

    hdrs = ["Date", "Event", "Detail", "Source"]
    for j, h in enumerate(hdrs):
        cell = ws.cell(row=4, column=1 + j, value=h)
        cell.font = COLHDR_FONT
        cell.fill = COLHDR_FILL

    talks_confirmed = (
        "Rio Tinto and Glencore confirmed preliminary talks on a "
        "possible all-share combination, structured as Rio Tinto "
        "acquiring Glencore via a UK scheme of arrangement. "
        "Combined group valued >$260bn at the time. Glencore "
        "shares rose 9.8% intraday to 453.50p."
    )
    walk_away = (
        "Rio Tinto abandoned talks, saying it could not reach "
        "terms that would 'deliver value to its shareholders'. "
        "Key sticking points: valuation methodology (Rio anchored "
        "to 7 Jan spot commodity prices; Glencore pushed for "
        "forward-looking/cycle pricing), governance and who would "
        "lead the combined group, and Glencore's undeveloped "
        "Argentine copper assets. Triggered a 6-month UK Takeover "
        "Code standstill on Rio approaching Glencore."
    )
    coal_rally = (
        "Coal prices and Glencore shares up ~26% since early "
        "January (vs. Rio Tinto shares +9% on weaker iron ore) "
        "shifted Glencore's implied ownership of a combined "
        "entity from ~31.5% toward its ~40% target, raising "
        "Glencore CEO Gary Nagle's hopes of reopening talks once "
        "the standstill lifted."
    )
    standstill_ends = (
        "The 6-month standstill lapsed this week. Reuters "
        "reported Rio Tinto insiders see no rush to re-engage -- "
        "CEO Simon Trott is focused on simplifying Rio Tinto into "
        "three core businesses, cost cuts and asset sales. "
        "Analyst: 'The ball is in Glencore's court. Any offer of "
        "value would have to be vastly different to the offer... "
        "discussed and rebuffed six months ago.'"
    )

    rows = [
        ("9 Jan 2026", "Talks confirmed", talks_confirmed,
         "Reuters/CNBC, 9 Jan 2026"),
        ("5 Feb 2026", "Rio Tinto walks away", walk_away,
         "Hargreaves Lansdown/Sharecast, 5 Feb 2026; "
         "Mining.com, 6 Feb 2026"),
        ("13 Mar 2026", "Coal rally revives hopes", coal_rally,
         "Hargreaves Lansdown/Sharecast, 13 Mar 2026"),
        ("4 Aug 2026", "Standstill expires", standstill_ends,
         "Reuters/Mining.com/MiningWeekly, 4 Aug 2026"),
    ]

    r = 5
    for date, event, detail, src in rows:
        ws.cell(row=r, column=1, value=date).font = LABEL_BOLD
        ws.cell(row=r, column=2, value=event).font = LABEL_BOLD
        c3 = ws.cell(row=r, column=3, value=detail)
        c3.font = LABEL_FONT
        c3.alignment = Alignment(wrap_text=True, vertical="top")
        c4 = ws.cell(row=r, column=4, value=src)
        c4.font = NOTE_FONT
        c4.alignment = Alignment(wrap_text=True, vertical="top")
        for col in range(1, 5):
            ws.cell(row=r, column=col).border = BORDER_ALL
            if col >= 3:
                ws.cell(row=r, column=col).alignment = Alignment(
                    wrap_text=True, vertical="top")
            else:
                ws.cell(row=r, column=col).alignment = Alignment(
                    vertical="top")
        ws.row_dimensions[r].height = 70
        r += 1

    sect(ws, r + 1, "DCF READ-THROUGH", "D")
    r += 3
    dcf_value_row = r
    lbl(ws, r, "Implied DCF value per share (GBp)", bold=True)
    link(ws, r, 2, "=DCF!$C$34", FMT_GBP_P, bold=True)
    r += 1
    jan_price_row = r
    lbl(ws, r, "Share price, 9 Jan 2026 (post +9.8% pop, "
               "pre-collapse)")
    inp(ws, r, 2, 453.50, FMT_GBP_P)
    note(ws, r, "Reuters/Sharecast, 9 Jan 2026.", 3)
    r += 1
    formula = "=B" + str(dcf_value_row) + "/B" + str(jan_price_row) + "-1"
    lbl(ws, r, "Implied premium/(discount) to 9 Jan 2026 price")
    frm(ws, r, 2, formula, FMT_PCT)
    r += 1
    current_price_row = r
    lbl(ws, r, "Share price, 7 Aug 2026 (latest close)")
    link(ws, r, 2, "=Assumptions!$B$5", FMT_GBP_P)
    r += 1
    formula = ("=B" + str(dcf_value_row) + "/B" +
               str(current_price_row) + "-1")
    lbl(ws, r, "Implied premium/(discount) to current price")
    frm(ws, r, 2, formula, FMT_PCT)
    r += 2

    reading_note = (
        "Reading the comparison: if the DCF value sits above the "
        "current price but (well) below the >$260bn valuation "
        "floated in January, that is consistent with Rio Tinto's "
        "stated reluctance to pay a control premium on top of "
        "Glencore's standalone worth -- rather than with Rio "
        "simply undervaluing the business. It does not resolve "
        "the two sides' actual disagreement, which centred on "
        "commodity-price methodology (spot vs. cycle-adjusted) "
        "and Glencore's Argentine copper assets -- items a "
        "standalone DCF, run off consensus-style blended "
        "assumptions, does not fully capture."
    )
    note(ws, r, reading_note, 1)
    ws.merge_cells(start_row=r, start_column=1, end_row=r + 1,
                    end_column=4)
    ws.cell(row=r, column=1).alignment = Alignment(
        wrap_text=True, vertical="top")
    ws.row_dimensions[r].height = 60


def build_cover(ws):
    ws.column_dimensions['A'].width = 40
    ws.column_dimensions['B'].width = 16
    ws.column_dimensions['C'].width = 58

    ws.cell(row=1, column=1, value="GLENCORE PLC (LSE: GLEN)"
            ).font = Font(name="Arial", size=18, bold=True,
                           color="1F3864")
    ws.cell(row=2, column=1,
            value="Discounted Cash Flow Analysis -- Standalone "
                  "Intrinsic Valuation"
            ).font = Font(name="Arial", size=12, italic=True,
                           color="404040")

    cover_intro = (
        "Prepared against the JPMorgan 2026 Global M&A Mid-Year "
        "Outlook -- cross-border, scale-driven consolidation "
        "theme -- using the Rio Tinto/Glencore approach "
        "(Jan-Aug 2026) as the live case study."
    )
    c = ws.cell(row=3, column=1, value=cover_intro)
    c.font = Font(name="Arial", size=9, italic=True, color="595959")
    c.alignment = Alignment(wrap_text=True)
    ws.merge_cells('A3:C3')
    ws.row_dimensions[3].height = 28
    ws.cell(row=4, column=1,
            value="Analyst: William   |   Date: 10 August 2026"
            ).font = Font(name="Arial", size=9, color="595959")

    sect(ws, 6, "KEY OUTPUTS", "C")
    rows_out = [
        ("Implied Enterprise Value ($mm)", "=DCF!$C$29", FMT_USD_MM),
        ("Implied Equity Value ($mm)", "=DCF!$C$31", FMT_USD_MM),
        ("Implied Value per Share ($)", "=DCF!$C$33", FMT_USD_PS),
        ("Implied Value per Share (GBp)", "=DCF!$C$34", FMT_GBP_P),
        ("Current Share Price (GBp)", "=Assumptions!$B$5", FMT_GBP_P),
        ("Implied Upside / (Downside)", "=DCF!$C$36", FMT_PCT),
        ("WACC", "=Assumptions!$B$27", FMT_PCT),
        ("Terminal Growth Rate", "=Assumptions!$B$30", FMT_PCT),
    ]
    r = 7
    for label, formula, fmt in rows_out:
        is_headline = label.startswith("Implied Value per Share (GBp")
        bold = is_headline or label.startswith("Implied Upside")
        lbl(ws, r, label, bold=is_headline)
        c = link(ws, r, 2, formula, fmt, bold=bold)
        if bold:
            c.fill = YELLOW_FILL
        r += 1

    r += 1
    sect(ws, r, "METHODOLOGY & KEY LIMITATIONS", "C")
    r += 1
    bullets = [
        "5-year explicit unlevered FCF forecast (FY2026E-FY2030E) "
        "off a FY2025A base, discounted at WACC; Gordon Growth "
        "terminal value, cross-checked against an implied exit "
        "EV/EBITDA multiple.",

        "EBITDA is grown with a single blended rate (fading 7% to "
        "3% p.a.), not a bottom-up, commodity-by-commodity "
        "price/volume build -- a simplification appropriate for a "
        "portfolio-level model but a real limitation versus how "
        "Glencore is covered on the Street.",

        "Glencore is a hybrid Marketing (trading) + Industrial "
        "(mining) business; this model values it on a single "
        "consolidated FCF basis rather than as a sum-of-the-parts, "
        "which would apply differentiated risk/multiples to each "
        "segment.",

        "Beta is genuinely uncertain -- sourced 5Y regression "
        "estimates for GLEN range from 0.51 to 1.21. The base "
        "case uses a peer-referenced 0.85; the Sensitivity tab "
        "spans WACC 6-12% so the reader is not anchored to one "
        "point estimate.",

        "Net debt, shares outstanding and FX are single recent "
        "data points, not forecast forward -- appropriate for a "
        "snapshot valuation, less so for tracking the position "
        "through time.",

        "This is a standalone DCF, not a merger/exchange-ratio "
        "model -- it does not value Rio Tinto or model the "
        "all-share exchange ratio the two sides actually "
        "negotiated over.",
    ]
    for b in bullets:
        c = ws.cell(row=r, column=1, value="- " + b)
        c.font = Font(name="Arial", size=9, color="000000")
        c.alignment = Alignment(wrap_text=True, vertical="top")
        ws.merge_cells(start_row=r, start_column=1, end_row=r,
                        end_column=3)
        ws.row_dimensions[r].height = 28
        r += 1

    r += 1
    sect(ws, r, "COLOUR LEGEND & TABS", "C")
    r += 1
    legend = [
        ("Blue text", "Hardcoded input / assumption -- change "
                      "these to run your own scenario."),
        ("Black text", "Formula -- recalculates automatically."),
        ("Green text", "Link to a figure on another tab."),
        ("Yellow fill", "Headline output or key assumption."),
    ]
    for k, v in legend:
        ws.cell(row=r, column=1, value=k).font = Font(
            name="Arial", size=9, bold=True)
        c = ws.cell(row=r, column=2, value=v)
        c.font = Font(name="Arial", size=9)
        c.alignment = Alignment(wrap_text=True)
        ws.merge_cells(start_row=r, start_column=2, end_row=r,
                        end_column=3)
        r += 1

    r += 1
    tabs_note = (
        "Tabs: Assumptions (all inputs) -> DCF (forecast & "
        "valuation bridge) -> Sensitivity (WACC x terminal "
        "growth) -> Deal Context (Rio Tinto timeline & "
        "read-through)."
    )
    c = ws.cell(row=r, column=1, value=tabs_note)
    c.font = Font(name="Arial", size=9, italic=True, color="595959")
    ws.merge_cells(start_row=r, start_column=1, end_row=r,
                    end_column=3)
    r += 2

    sources_note = (
        "Primary sources: Glencore FY2025 Preliminary Results & "
        "H1 2025 Report (glencore.com); Reuters, CNBC, Mining.com, "
        "Hargreaves Lansdown/Sharecast on the Rio Tinto talks; "
        "Yahoo Finance, MarketBeat, TradingEconomics for market "
        "data. Full citations on the Assumptions and Deal Context "
        "tabs. Educational/portfolio use -- not investment advice."
    )
    c = ws.cell(row=r, column=1, value=sources_note)
    c.font = NOTE_FONT
    c.alignment = Alignment(wrap_text=True)
    ws.merge_cells(start_row=r, start_column=1, end_row=r,
                    end_column=3)
    ws.row_dimensions[r].height = 40


def polish(wb, ws_cover, ws_a, ws_d, ws_s, ws_m):
    ws_cover.sheet_view.showGridLines = False
    ws_cover.sheet_properties.tabColor = "1F3864"
    ws_a.sheet_properties.tabColor = "2E5395"
    ws_d.sheet_properties.tabColor = "2E5395"
    ws_s.sheet_properties.tabColor = "548235"
    ws_m.sheet_properties.tabColor = "833C00"

    ws_a.freeze_panes = "B5"
    ws_d.freeze_panes = "B5"
    ws_s.freeze_panes = "C5"
    ws_m.freeze_panes = "A5"

    for sheet in [ws_a, ws_d, ws_s, ws_m]:
        sheet.sheet_view.zoomScale = 100

    wb.active = 0


if __name__ == "__main__":
    build()
