# BP

    Code
      format(x)
    Output
      [1] "-28050 CE" "-33050 CE" "-38050 CE"

---

    Code
      format(x, format = "ka")
    Output
      [1] "-28.05 ka CE" "-33.05 ka CE" "-38.05 ka CE"

---

    Code
      format(x, format = "Ma")
    Output
      [1] "-0.02805 Ma CE" "-0.03305 Ma CE" "-0.03805 Ma CE"

---

    Code
      format(x, format = "Ga")
    Output
      [1] "-2.805e-05 Ga CE" "-3.305e-05 Ga CE" "-3.805e-05 Ga CE"

---

    Code
      format(x, format = TRUE)
    Output
      [1] "-28.05 ka CE" "-33.05 ka CE" "-38.05 ka CE"

