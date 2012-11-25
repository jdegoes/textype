package com.github.textype

object CSVSamples {
  val StandardEncoding1 = 
  """transaction_unique_identifier,seller_company_name,customer_company_name,customer_duns_number,tariff_reference,contract_service_agreement,trans_id,transaction_begin_date,transaction_end_date,time_zone,point_of_delivery_control_area,specific location,class_name,term_name,increment_name,increment_peaking_name,product_name,transaction_quantity,price,units,total_transmission_charge,transaction_charge
T1,The Electric Company,"The Electric Marketing Co., LLC",23456789,FERC Electric Tariff Original Volume No. 2,Service Agreement 1,8700,200401010000,200403312359,ES,PJM,BUS 4321,UP,LT,Y,FP,ENERGY,22574,39,$/MWH,0,880386
T2,The Electric Company,The Power Company,45653333,FERC Electric Tariff Original Volume No. 10,2,8701,200401010000,200402010000,CS,DPL,Green Sub Busbar,F,ST,M,FP,ENERGY,16800,32,$/MWH,0,537600
T3,The Electric Company,The Power Company,45653333,FERC Electric Tariff Original Volume No. 10,2,8702,200402010000,200403010000,CS,DPL,Green Sub Busbar,F,ST,M,FP,ENERGY,16800,32,$/MWH,0,537600
T4,The Electric Company,The Power Company,45653333,FERC Electric Tariff Original Volume No. 10,2,8703,200403010000,200404010000,CS,DPL,Green Sub Busbar,F,ST,M,FP,ENERGY,16800,32,$/MWH,0,537600
T5,The Electric Company,The Power Company,45653333,FERC Electric Tariff Original Volume No. 10,2,8704,200401111600,200401121559,CS,AEP,Tile Busbar,F,ST,D,FP,ENERGY,1200,50,$/MWH,0,60000
T6,The Electric Company,The Power Company,45653333,FERC Electric Tariff Original Volume No. 10,2,8801,200402011300,20040215061900,ES,HUB,Entergy (into),NF,ST,H,FP,ENERGY,1875,13.75,$/MWH,6675,32456.25
T7,The Electric Company,The Power Company,45653333,FERC Electric Tariff Original Volume No. 10,2,8802,20040202110000,200402021800,ES,HUB,PJM-W,NF,ST,H,FP,BOOKED OUT POWER,350,32,$/MWH,0,11200
T8,The Electric Company,The Power Company,45653333,FERC Electric Tariff Original Volume No. 10,2,8803,20040203060500,20040210112200,ES,HUB,PJM-W,NF,ST,H,FP,ENERGY,1875,44,$/MWH,0,82500
T9,The Electric Company,The Power Company,45653333,FERC Electric Tariff Original Volume No. 10,2,8804,20040211062300,20040219081200,PP,HUB,Four Corners,NF,ST,H,FP,ENERGY,1875,48,$/MWH,0,90000
T10,The Electric Company,The Power Company,45653333,FERC Electric Tariff Original Volume No. 10,132,7125,20040109081100,20040309082300,CS,HUB,AEP (into),F,ST,H,OP,ENERGY,150,22,$/MWH,4264,7564
T10,The Electric Company,The Power Company,45653333,FERC Electric Tariff Original Volume No. 10,132,7125,20040201010000,20040225080800,CS,HUB,AEP (into),F,ST,H,OP,ENERGY,150,28,$/MWH,0,4200
T10,The Electric Company,The Power Company,45653333,FERC Electric Tariff Original Volume No. 10,132,7125,20040108111100,20040301121200,CS,HUB,AEP (into),F,ST,H,OP,ENERGY,150,44,$/MWH,0,6600
T10,The Electric Company,The Power Company,45653333,FERC Electric Tariff Original Volume No. 10,132,7125,20040322112200,20040325120000,CS,HUB,AEP (into),F,ST,H,OP,ENERGY,150,44,$/MWH,0,6600
T10,The Electric Company,The Power Company,45653333,FERC Electric Tariff Original Volume No. 10,132,7125,20040323191900,20040328191700,CS,HUB,AEP (into),F,ST,H,OP,ENERGY,150,58,$/MWH,0,8700
T10,The Electric Company,The Power Company,45653333,FERC Electric Tariff Original Volume No. 10,132,7125,20040111010000,20040122022200,CS,HUB,AEP (into),F,ST,H,OP,CAPACITY,150,20,$/MW-DAY,0,3000
T11,The Electric Company,Utility A,38495837,FERC Electric Tariff Original Volume No. 10,15,8711,200403150800,200403150859,ES,ISNE,NEPOOL Mass HUB,F,ST,H,P,Energy,100,50,$/MWH,0,5000
T11,The Electric Company,Utility A,38495837,FERC Electric Tariff Original Volume No. 10,15,8711,200403150800,200403150859,ES,ISNE,NEPOOL Mass HUB,F,ST,H,P,Energy,-10,60,$/MWH,0,-600
T12,The Electric Company,Utility A,38495837,FERC Electric Tariff Original Volume No. 10,15,8711,200403150900,200403150959,ES,ISNE,NEPOOL Mass HUB,F,ST,H,P,Energy,100,55,$/MWH,0,5500
T12,The Electric Company,Utility A,38495837,FERC Electric Tariff Original Volume No. 10,15,8711,200403150900,200403150959,ES,ISNE,NEPOOL Mass HUB,F,ST,H,P,Energy,-5,59,$/MWH,0,-295
T13,The Electric Company,Utility A,38495837,FERC Electric Tariff Original Volume No. 10,15,8711,200403151000,200403151059,ES,ISNE,NEPOOL Mass HUB,F,ST,H,P,Energy,100,62,$/MWH,0,6200
T13,The Electric Company,Utility A,38495837,FERC Electric Tariff Original Volume No. 10,15,8711,200403151000,200403151059,ES,ISNE,NEPOOL Mass HUB,F,ST,H,P,Energy,-10,60,$/MWH,0,-600
T14,The Electric Company,Utility A,38495837,FERC Electric Tariff Original Volume No. 10,15,8711,200403151100,200403151159,ES,ISNE,NEPOOL Mass HUB,F,ST,H,P,Energy,100,62,$/MWH,0,6200
T14,The Electric Company,Utility A,38495837,FERC Electric Tariff Original Volume No. 10,15,8711,200403151100,200403151159,ES,ISNE,NEPOOL Mass HUB,F,ST,H,P,Energy,-10,59,$/MWH,0,-590
T15,The Electric Company,Utility B,493758794,FERC Electric Tariff Original Volume No. 10,7,8712,200402140200,200402140259,EP,NYIS,Zone A,F,ST,H,FP,Booked out power,60,30,$/MWH,0,1800"""
}