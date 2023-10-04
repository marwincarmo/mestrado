library(qrcode)

# set.seed() value omitted (its my birthday)
# get always the same image. good in case i need to change the url in the future

postercode_spaq <- qr_code("https://drive.google.com/file/d/1Fxe_Fg9klDEf1vsXn1K4q5cuWV2RAwKg/view?usp=sharing", "M")

generate_svg(
  postercode_spaq,
  "conferences/world-sleep/img/qrcodeSPAQ.svg",
  size = 300,
  foreground = "white",
  background = "#024F84"
)

postercodeNW <- qr_code("https://drive.google.com/file/d/1HOF2369qjca1tjSuhUS-HOd8P_u8IGEk/view?usp=sharing", "M")

generate_svg(
  postercodeNW,
  "conferences/cbsono_22/img/qrcodeNW.svg",
  size = 300,
  foreground = "white",
  background = "#024F84"
)
