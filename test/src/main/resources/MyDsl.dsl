Record baseAmount {
     Field required variety:        C          pos ( 1,  1) as fixedValue C
     Field required type:           C          pos (42,  1) as fixedValue 0

     Field required conditionId:    Zc         pos ( 2,  3) as fixedValue 000

     Field sign:                    C          pos (45,  1) as enum { +, - }
     Field amount:                  N (10, 5)  pos (46, 15)
}

Record condition {
     Field required variety:        C          pos ( 1,  1) as fixedValue C
     Field required type:           C          pos (42,  1) as fixedValue 1

}

