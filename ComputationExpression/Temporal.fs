module Temporal
    type DateTime = System.DateTime
    type HalfOpenPeriod = { startDate:DateTime; endDate:DateTime }
    type Temporary<'a> = { period: HalfOpenPeriod; value:'a }

    let jan15 d = DateTime(2015,1,d)

    let (==>) dateFrom dateTo = { startDate = dateFrom; endDate = dateTo }
    let (:=) period value = { period = period; value = value }