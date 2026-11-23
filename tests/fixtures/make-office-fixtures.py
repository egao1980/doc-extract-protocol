"""Regenerate the Office fixtures for tests/office-test.lisp (issue #6).

    uv run --with openpyxl==3.1.5 --with python-docx==1.1.2 \
        python -I tests/fixtures/make-office-fixtures.py tests/fixtures

openpyxl-report.xlsx  openpyxl: inline strings (t="inlineStr"), no sharedStrings.xml
excel-report.xlsx     Excel layout: shared strings by index, numbers, a formula
                      with its cached value, two sheets
python-docx-report.docx  heading, a paragraph split into runs mid-word,
                      an entity (R&D), a 2x2 table
"""
import os
import sys
import zipfile

out = sys.argv[1]

from openpyxl import Workbook

wb = Workbook()
ws = wb.active
ws.title = "Sales"
for row in [["region", "q1", "q2"], ["North", 120, 135], ["South", 98, 110]]:
    ws.append(row)
wb.save(os.path.join(out, "openpyxl-report.xlsx"))

# Excel-written shape, kept minimal: openpyxl cannot write sharedStrings or
# cached formula values, so the parts are written by hand.
CT = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types"><Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/><Default Extension="xml" ContentType="application/xml"/><Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/><Override PartName="/xl/worksheets/sheet1.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/><Override PartName="/xl/worksheets/sheet2.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/><Override PartName="/xl/sharedStrings.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sharedStrings+xml"/></Types>"""
RELS = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"><Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/></Relationships>"""
WB = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"><sheets><sheet name="Totals" sheetId="1" r:id="rId1"/><sheet name="Notes" sheetId="2" r:id="rId2"/></sheets></workbook>"""
WB_RELS = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"><Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet1.xml"/><Relationship Id="rId2" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet2.xml"/><Relationship Id="rId3" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/sharedStrings" Target="sharedStrings.xml"/></Relationships>"""
# Shared strings deliberately not in cell order: index 0 is "North".
SST = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<sst xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" count="5" uniqueCount="5"><si><t>North</t></si><si><t>region</t></si><si><t>total</t></si><si><r><rPr><b/><vertAlign val="superscript"/></rPr><t>q</t></r><r><t>2</t></r></si><si><t>R&amp;D only</t></si></sst>"""
SHEET1 = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"><sheetData><row r="1"><c r="A1" t="s"><v>1</v></c><c r="B1" t="s"><v>3</v></c><c r="C1" t="s"><v>2</v></c></row><row r="2"><c r="A2" t="s"><v>0</v></c><c r="B2"><v>135</v></c><c r="C2"><f>B2*2</f><v>270</v></c></row></sheetData><tableParts count="0"/></worksheet>"""
SHEET2 = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"><sheetData><row r="1"><c r="A1" t="s"><v>4</v></c></row></sheetData></worksheet>"""
with zipfile.ZipFile(os.path.join(out, "excel-report.xlsx"), "w", zipfile.ZIP_DEFLATED) as z:
    z.writestr("[Content_Types].xml", CT)
    z.writestr("_rels/.rels", RELS)
    z.writestr("xl/workbook.xml", WB)
    z.writestr("xl/_rels/workbook.xml.rels", WB_RELS)
    z.writestr("xl/sharedStrings.xml", SST)
    z.writestr("xl/worksheets/sheet1.xml", SHEET1)
    z.writestr("xl/worksheets/sheet2.xml", SHEET2)

import docx

d = docx.Document()
d.add_heading("Quarterly report", 1)
p = d.add_paragraph()
p.add_run("Sales gr")
p.add_run("ew").bold = True
p.add_run(" in both regions.")
d.add_paragraph("R&D grew too.")
t = d.add_table(rows=2, cols=2)
t.cell(0, 0).text = "region"
t.cell(0, 1).text = "q2"
t.cell(1, 0).text = "North"
t.cell(1, 1).text = "135"
d.save(os.path.join(out, "python-docx-report.docx"))
print("ok")
