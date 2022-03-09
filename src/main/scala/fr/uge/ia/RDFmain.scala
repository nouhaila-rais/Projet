package fr.uge.ia

object RDFmain extends App {
  val db = Lubm(LabelBase.INPUT)
  db.load()
  //db.listAllTypes()
  //db.listAllFullProfessor()
  //db.generate_gender()
  //db.generate_vaccin()
  //db.generateInfo()
  db.addInfoToPersons()
  db.addInfoVaccinToPersons()
  //db.gen()
  db.toJsonVaccin()
}