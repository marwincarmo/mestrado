library(qrcode)

# set.seed() value omitted (its my birthday)
# get always the same image. good in case i need to change the url in the future

postercode_sas <- qr_code("https://drive.google.com/file/d/1aar-tISxi9mH1dqe5vnEKy7PIRJCkc5A/view?usp=sharing", "M")

generate_svg(
  postercode_sas,
  "conferences/cbsono_22/img/qrcodeSAS.svg",
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
