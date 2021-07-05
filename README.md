# stock-buy-hold-sell-classification
This is my project for Data Career Day at Algoritma School of Data Sciece.
# Background

<b>Is today the right time for you to invest in stock?</b>

Jika ditanya apakah hari ini anda sudah berinvestasi? Kebanyakan orang Indonesia pasti akan menjawab belum. Hal tersebut bisa terjadi dikarenakan kebanyakan masyarakat kurang paham akan pentingnya dan manfaat yang bisa di dapatkan dari investasi. Padahal jika yang dilakukan hanya dengan menabung, menabung saja tidak akan cukup karena adanya pengaruh dari inflasi dan rata-rata inflasi yang terjadi di Indonesia sampai dengan 5% per tahun, apakah bunga yang didapat dari menabung sudah bisa menutupi rata-rata inflasi yang terjadi? Jawabannya belum tentu, dikarenakan rata-rata bunga yang diberikan dari lembaga keungan di Indoensia sebesar 4%. Oleh karena itu investasi juga diperlukan agar nilai dari aset-aset yang dimiliki bisa bertumbuh tanpa khawatir tergerus oleh inflasi. 

Pilihan investasi yang dapat dipilih juga beragam antara lain investasi pasar modal(contoh produknya adalah saham, reksa dana, obligasi/surat hutang, <i>Exchange Traded Fund</i>), investasi emas (contoh produknya adalah emas batangan, emas perhiasan) dan invetasi properti (contoh produknya adalah tanah, rumah, apartamen).

Dalam projek ini akan fokus ke investasi pasar modal yaitu saham. Saham dapat didefinisikan sebagai tanda penyertaan modal seseorang atau pihak (badan usaha) dalam suatu perusahaan atau perseroan terbatas. Dengan menyertakan modal tersebut, maka pihak tersebut memiliki klaim atas pendapatan perusahaan, klaim atas asset perusahaan, dan berhak hadir dalam Rapat Umum Pemegang Saham (RUPS). Akan tetapi tidak semua perusahaan di Indoensia bisa dibeli sahamnya karena tidak semua perusahaan dibuka secara umum, keputsan sebuah perusahaan ingin menjual sahamnya atau tidak merupakan keputusan sepihak dari perusahaan tersebut maka dari itu tidak semua perusahaan bisa dibeli sahamnya.

![*Source: Instagram - @indonesiastockexchange](C:\Users\user\Desktop\Algoritma\DCD\Scraping Stock Data\Proposal Final\jumlah investor saham.png)

Pada 17 Feburari 2021 lalu, Bursa Efek Indonesia (BEI) menyatakan bahwa total investor saham di Indonesia sudah mencapai 2 juta Single Investor Identification (SID). Dari pernyataan BEI timbulah peratanyaan, apakah angka investor saham yang sudah mencapai 2 juta SID sudah banyak dari total penduduk Indonesia yang sampai dengan 270 juta? Jawabannya tentu saja tidak, karena itu hanya sekitar 0.8% dari total penduduk Indonesia. Hal tersebut bisa terjadi selain kurangnya pemahaman tentang pentingnya berinvestasi, dikarenakan kebanyakan masyarakat Indonesia merasa kesulitan untuk memahami teori analisa yang dibutuhkan ketika berinvestasi di saham. 

Pada dunia saham ada dua teori analisa yang biasanya digunakan yaitu analisa fundamental dan analisa teknikan, pemahaman akan kedua teori tersebut memang sangat penting dikarenakan dari pemahaman teori analisa fundamental bisa mendapatkan infomasi saham apa yang masih layak untuk di investasikan dan dari pemahaman teori analisa teknikal bisa mendapatkan informasi kapan waktu yang tepat untuk membeli maupun menjual saham tersebut untuk mendapatkan keuntungan yang maksimal. Akan tetapi untuk memahami mengenai kedua teori tersebut tidak jarang membutuhkan waktu yang tidak singkat.

Projek yang akan dikerjakan ini akan menghasilkan sebuah prediksi yang dapat digunakan sebagai rekomendasi untuk para investor saham mengenai apakah hari ini adalah waktu yang tepat untuk membeli atau menjual saham yang sudah dikurasi berdasarkan hasil analisa teknikal. Harapannya projek ini  dapat dimanfaatkan oleh <i>broker</i> (penyedia jasa untuk menghubungan investor dengan pasar modal) menjawab kesulitan ataupun keraguan masyarakat Indonesia untuk memulai berinvestasi di saham, selain untuk para investor pemula diharapkan juga projek ini dapat membantu para investor yang sudah berpengalaman.

# Problem Statement

Keraguan para masyarakat Indonesia untuk berinvestasi di saham dikarenakan kurangnya pemahaman teori analisa yang diperlukan. Didalam dunia saham sendiri ada 2 tipe analsia yang dapat dilakukan yaitu analisa fundamental dan analisa teknikal. 

- <b>Analisa fundamental</b> biasanya dilakukan paling pertama dengan membaca laporan keuangan dari badan usaha tersebut, dari laporan keuangan tersebut para investor bisa melihat performa badan usaha tersebut dari segi produk yang disajikan, aset yang dimiliki, keuntungan atau beban yang dirasakan  dan mengenai kebijakan terkait selama periode waktu tertentu.
- <b>Analisa teknikal</b> adalah analisa lanjutan setelah memilih sebuah saham tertentu untuk di investasikan. Teknik analisis ini merupakan metode yang digunakan lewat pengamatan pola-pola data pada pasar saham. Pada praktiknya, analisis teknikal saham bakal mendorong investor untuk fokus dalam memperhatikan pergerakan harga pasar.

Dari penjelasan mengenai 2 teori yang diperlukan untuk melakukan analisa didalam dunia saham, analisa fundamental pastinya akan lebih mudah dilakukan karena seluruh masyrakat Indonesia dapat melakukan analisa dari laporan keuangan yang sudah dikurasi dengan rapi dan dapat diakses dengan mudah dari website badan usaha tersebut. Untuk mendapatkan informasi lebih mengenai badan usaha tersebut, selain melihat laporan keuangan para investor juga bisa membaca berita terakit mengenai badan usaha tersebut apakah sepak terjangnya cukup bagus. Dari informasi yang sudah didapatkan para investor bisa berfikir kritis untuk menentukan apakah saham tersebut masih layak untuk diintestasikan atau tidak

Sedangkan analisa teknikal lebih susah dilakukan dikarenakan analisa ini tidak semudah membaca laporan keungan yang sudah dituliskan secara rapi dan jelas seperti analisa fundamental karena tahapan ini memerlukan banyak indikator dan disetiap indikator membutuhkan perhitungan matematis yang berbeda dari pergerakan harga saham setiap harinya. Dengan pemahaman analisa ini para investor dapat menentukan kapan waktu yang tepat untuk membeli dan menjual saham untuk mendapatkan keuntungan yang maksimal.

Akan tetapi kebanyakan masyarkat Indonesia tidak berani berinvestasi disaham dikarenakan kurangnya pemahaman kedua analisa yang dibutuhkan terutama analisa teknikal, banyak yang berpendapat bahwa cukup mudah untuk menentukan saham apa yang bagus tapi sangat susah untuk menentukan kapan waktu yang tepat untuk menjual atau membeli saham tersebut karena tidak paham untuk melakukan analisa teknikal dan sudah takut diawal sebelum mempelajarinya karena mengetahui banyak rumus dan teori yang harus dipahami.

# Problem Scope

Pada projek ini akan menarik data dari <i>Yahoo Finance</i> dengan menggunakan fungsi `tq_get()` dari library `tidyquant`.

Dibawah ini adalah 5 sektor saham berserta pilihan saham apa saja yang akan dipilih salah satunya untuk masing-masing sektor:
<br>- <b>Sektor Bank:</b> Bank Rakyat Indonesia (BBRI.JK), Bank Central Asia (BBCA.JK), Bank Mandiri (BMRI.JK), Bank Tabungan Pensiunan Negara(BTPN.JK), Bank Jago (ARTO.JK).
<br>- <b>Sektor Telekomunikasi:</b> Telkom Indonesia (TLKM.JK),XL Axiata (EXCL.JK), Smartfren Telecom (FREN.JK), Indosat (ISAT.JK).
<br>- <b>Sektor Makanan & Minuman:</b> Indofood (INDF.JK), Unilever (UNVR.JK), Japfa Comfeed Indonesia (JPFA.JK), Mayora (MYOR.JK), Ultrajaya Milk Industry (ULTJ.JK).
<br>- <b>Sektor Farmasi:</b> Kalbe Farma (KLBF.JK), Sido Muncul (SIDO.JK), Kimia Farma (KAEF.JK), Indofarma (INAF.JK).
<br>- <b>Sektor Infrastruktur:</b> Waskita Karya (WSKT.JK), Wijaya Karya (WIKA.JK), Adhi Karya (ADHI.JK), Bukit Asam (PTBA.JK), Pembangungan Perumahan (PTPP.JK).

Pemilihan 5 sektor saham diatas berdasarkan potensi yang ditawarkan dalam jangka panjang maupun pendek ditambah juga dengan pertimbangan ke 5 sektor tersebut merupakan sektor yang memberikan kebutuhan yang tidak dapat dilepaskan dari masyrakat Indonesia. Untuk pemilihan sahamnya sendiri sudah dikurasi untuk mengambil saham-saham dari perusahaan yang sudah terbukti sepak terjangnya di Indonesia, dari saham-saham per-sektornya nanti akan dicari lagi manakah saham yang paling baik sebagai perwakilan dari sektor tersebut.

Dari nama saham-saham diatas akan mengandung informasi sebagai berikut:
<br>- `symbol`: singkatan/simbol dari stock yang akan diprediksi.
<br>- `date`: histori tanggal harga stock.
<br>- `open`: posisi harga saham per lembarnya pada saat pembukaan yang terjadi pada hari tersebut.
<br>- `high`: posisi harga saham per lembarnya pada tertinggi yang terjadi pada hari tersebut.
<br>- `low`: posisi harga saham per lembarnya pada terendah yang terjadi pada hari tersebut.
<br>- `close`: posisi harga saham per lembarnya pada terakhir yang terjadi pada hari tersebut.
<br>- `volume`: total jumlah volume yang ada pada hari tersebut.
<br>- `adjusted`: sama dengan posisi harga saham per lembarnya pada terakhir yang terjadi pada hari tersebut tapi disesuaikan dengan pembagian deviden yang berlaku.

Untuk kebutuhan membuat model, setelah mendapatkan 8 infromasi diatas untuk masing-masing saham dari <i>Yahoo Finance</i>, ada beberapa hal lagi yang dilakukan untuk mendapatkan <i>target variable</i> yang merupakan label <i>Buy</i>, <i>Hold</i> atau <i>Sell</i>. Proses untuk mendapatkan target variable yang diinginkan berdasarkan hasil analisa teknikal yang akan dilakukan di bagian Exploratory Data Analysis dibagian bawah nanti.

# Project Idea

Ide dari permasalahan yang ada adalah mengembangkan sebuah model machine learning yang dapat melakukan klasifikasi apakah hari ini saat yang tepat untuk "Buy", "Sell" atau "Hold" saham tertentu berdasarkan hasil dari beberapa teknikal analisis, hasil klasifikasi dari model yang dibuat dapat dimanfaatkan oleh para investor terutama investor baru untuk mengambil keputusan kapan harus membeli ataupun menjual saham tertentu agar mendapatkan keuntung semaksimal mungkin.

# Output

Output yang akan dihasilkan berupa dashboard yang akan dibuat dengan R Shinny. Dashboard tersebut akan berisikan 2 bagian yaitu bagian <i>Prediction</i> dan <i>Simulation</i>.

<b>Bagian Prediction:</b>

Pada bagian prediction akan menampilkan inti dari tujuan projek ini yaitu berupaa rekomendasi dari hasil prediksi apakah hari ini harus "Buy", "Sell" atau "Hold" saham tersebut berdasarkan beberapa analisa teknnikal yang dilakukan dan akan disimpulkan dengan barometer. Dibagian ini juga akan menampilkan pergerakan harga saham dalam bentuk grafik berserta dengan tambahan informasti mengenai saham tersebut.

![*Dashboard bagian prediction](C:/Users/user/Desktop/Algoritma/DCD/Scraping Stock Data/Proposal Final/dashboard prediction.png)

<b>Bagian Simulation:</b>

Pada bagian simulation akan menunjukan manfaat yang didaptkan jika menggunakan machine learning untuk prediksi berdasarkan analisa teknikal yang akan dibandingkan tanpa menggunakan machine learning dan analisa teknikal apapun. 

 ![*Dashboard bagian simulation](C:/Users/user/Desktop/Algoritma/DCD/Scraping Stock Data/Proposal Final/dashboard simulation.png)

# Business Impact

Projek ini dapat dikembangkan untuk memberikan manfaat kepada 3 pihak, yaitu broker, investor dan negara.
<br>- Broker dapat memanfaatkan projek ini sebagai salah satu fitur untuk menarik para investor baru untuk berinvestasi saham.
<br>- Investor(individu) dapat memanfaatkan projek ini untuk mendapatkan penghasilan tambahan untuk menghadapi inflasi.
<br>- Negara juga akan mendapatkan manfaat dari projek in karena dengan bertambahnya jumlah investor, maka semakin banyak juga pendapatan negara tersebut.
