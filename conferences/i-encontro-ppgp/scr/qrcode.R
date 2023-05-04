library(qrcode)

# set.seed() value omitted (its my birthday)
# get always the same image. good in case i need to change the url in the future

postercode_sas <- qr_code("https://drive.google.com/file/d/1HQAPixYpBuOGqJl40jW8OBmMoP44idrd/view?usp=sharing", "M")

generate_svg(
  postercode_sas,
  "conferences/i-encontro-ppgp/img/qrcodeCADS.svg",
  size = 300,
  foreground = "white",
  background = "#024F84"
)
