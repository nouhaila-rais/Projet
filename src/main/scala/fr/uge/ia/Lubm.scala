package fr.uge.ia

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.github.javafaker.Faker
import org.apache.jena.ontology.{OntClass, OntModelSpec}
import org.apache.jena.rdf.model.{ModelFactory, Resource, ResourceFactory}

import java.io.{File, FileOutputStream, StringWriter}
import java.util
import java.util.concurrent.TimeUnit
import java.util.{Date, Locale, Random}



sealed trait Gender {
  val name : String
}

case object Male extends Gender{ override val name = "Male" }
case object Femelle extends Gender{override val name = "Femelle"}

sealed trait Vaccin {
  val name : String
}

case object Pfizer extends Vaccin{override val name = "Pfizer"}
case object Moderna extends Vaccin{override val name = "Moderna"}
case object AstraZeneca extends Vaccin{override val name = "AstraZeneca"}
case object SpoutnikV extends Vaccin{override val name = "SpoutnikV"}
case object CanSinoBio extends Vaccin {override val name = "CanSinoBio"}

sealed trait SideEffect {
  val SIDER_code : String
  val name : String
}

case object Injection_site_pain extends SideEffect{ override val SIDER_code: String = "C0151828"; override val name: String = "Injection site pain"}
case object fatigue extends SideEffect{ override val SIDER_code: String = "C0015672"; override val name: String = "fatigue"}
case object headache extends SideEffect{ override val SIDER_code: String = "C0018681"; override val name: String = "headache"}
case object Muscle_pain extends SideEffect{ override val SIDER_code: String = "C0231528"; override val name: String = "Muscle pain"}
case object chills extends SideEffect{ override val SIDER_code: String = "C0085593"; override val name: String = "chills"}
case object Joint_pain extends SideEffect{ override val SIDER_code: String = "C0003862"; override val name: String = "Joint pain"}
case object fever extends SideEffect{ override val SIDER_code: String = "C0015967"; override val name: String = "fever"}
case object Injection_site_swelling extends SideEffect{ override val SIDER_code: String = "C0151605"; override val name: String = "Injection site swelling"}
case object Injection_site_redness extends SideEffect{ override val SIDER_code: String = "C0852625"; override val name: String = "Injection site redness"}
case object Nausea extends SideEffect{ override val SIDER_code: String = "C0027497"; override val name: String = "Nausea"}
case object Malaise extends SideEffect{ override val SIDER_code: String = "C0231218"; override val name: String = "Malaise"}
case object Lymphadenopathy extends SideEffect{ override val SIDER_code: String = "C0497156"; override val name: String = "Lymphadenopathy"}
case object Injection_site_tenderness extends SideEffect{ override val SIDER_code: String = "C0863083"; override val name: String = "Injection site tenderness"}

class Lubm  (val dbSource: String){
  val faker = new Faker(new Locale("en-US"))

  val model = ModelFactory.createDefaultModel()

  def load() =  model.read(dbSource,"TTL")
  def showModel() : Unit =  println("is empty ? "+model.isEmpty())
  def size() = model.size()
  val typeProperty = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
  def listAllTypes() = {
    val inf = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM_MICRO_RULE_INF)
    inf.read("file:///home/2ind2/loic.jouanisson/univ-bench.owl")
    val pers = inf.getOntClass("http://swat.cse.lehigh.edu/onto/univ-bench.owl#Person")
    val it = pers.listSubClasses(false)
    val itList = it.filterDrop(s => s.toString.contains("http://www.w3.org/2002/07/owl#Nothing")).toList
    itList
  }
  val types = listAllTypes()


  // Mettre tous les subject iterator dans la list
  def listAllPerson() = {
    val rdfType = model.createProperty(typeProperty)
    val list = List()
    types.forEach( obj => {
      val personObj = model.createResource(obj)
      val it = model.listSubjectsWithProperty(rdfType, personObj)
      list.appended(it)
    })
    list
  }

  def listAllMale() = {
    val rdfType = model.createProperty("http://swat.cse.lehigh.edu/onto/univ-bench.owl#gender")
    val it = model.listSubjectsWithProperty(rdfType,Male.name)
    it.toList
  }

  def listAllFemale() = {
    val rdfType = model.createProperty("http://swat.cse.lehigh.edu/onto/univ-bench.owl#gender")
    val it = model.listSubjectsWithProperty(rdfType,Femelle.name)
    it.toList
  }

  def listAllVaccinated() = {
    val rdfType = model.createProperty("http://swat.cse.lehigh.edu/onto/univ-bench.owl#vaccinated")
    val it = model.listSubjectsWithProperty(rdfType)
    it.toList
  }

  def generate_gender(): Gender ={
    val rand = new Random()
    rand.nextInt(2)  match {
      case 0 => Male
      case 1 => Femelle
    }
  }

  def generate_vaccin(): Vaccin ={
    val rand = new Random()
    rand.nextInt(5) match {
      case 0 => Pfizer
      case 1 => Moderna
      case 2 => AstraZeneca
      case 3 => SpoutnikV
      case 4 => CanSinoBio
    }
  }

  def generate_side_effect(): SideEffect ={
    val rand = new Random()
    rand.nextInt(13) match {
      case 0 => Injection_site_pain
      case 1 => fatigue
      case 2 => headache
      case 3 => Muscle_pain
      case 4 => chills
      case 5 => Joint_pain
      case 6 => fever
      case 7 => Injection_site_swelling
      case 8 => Injection_site_redness
      case 9 => Nausea
      case 10 => Malaise
      case 11 => Lymphadenopathy
      case 12 => Injection_site_tenderness

    }
  }

  def generateInfo() ={
    val id  = faker.idNumber().valid()
    val fname = faker.name().firstName()
    val lname = faker.name().lastName()
    val birthdate = faker.date().birthday(30, 70)
    val gender = generate_gender()
    val vaccin = generate_vaccin()
    //System.out.println("" +fname)
    (fname, lname,birthdate,gender,vaccin)
  }

  def addInfoToPersons()={
    val persons = listAllPerson()
    var i:Int =0;
    var info=generateInfo()
    var id = 0L
    persons.foreach(person => {
      System.out.println(person.toString)
    })/*
    persons.forEach(person => {
      val fname = faker.name().firstName()
      val lname = faker.name().lastName()
      val birthdate = faker.date().birthday(30, 70)
      val gender = generate_gender()
      //System.out.println("" +fname)

      val propertyId = ResourceFactory.createProperty("http://swat.cse.lehigh.edu/onto/univ-bench.owl#id")
      val propertyFname = ResourceFactory.createProperty("http://swat.cse.lehigh.edu/onto/univ-bench.owl#firstName")
      val propertyLname = ResourceFactory.createProperty("http://swat.cse.lehigh.edu/onto/univ-bench.owl#lastName")
      val propertybirthdate = ResourceFactory.createProperty("http://swat.cse.lehigh.edu/onto/univ-bench.owl#birthdate")
      val propertygender = ResourceFactory.createProperty("http://swat.cse.lehigh.edu/onto/univ-bench.owl#gender")

      val objectId = ResourceFactory.createStringLiteral(id.toString)
      val objectFname = ResourceFactory.createStringLiteral(fname)
      val objectLname = ResourceFactory.createStringLiteral(lname)
      val objectbirthdate =ResourceFactory.createStringLiteral(birthdate.toString)
      val objectgender =ResourceFactory.createStringLiteral(gender.name)

      val idStmt = ResourceFactory.createStatement(person, propertyId, objectId)
      val fnameStmt = ResourceFactory.createStatement(person, propertyFname, objectFname)
      val lnameStmt = ResourceFactory.createStatement(person, propertyLname, objectLname)
      val birthdateStmt = ResourceFactory.createStatement(person, propertybirthdate, objectbirthdate)
      val genderStmt = ResourceFactory.createStatement(person, propertygender, objectgender)

      model.add(idStmt)
      model.add(fnameStmt)
      model.add(lnameStmt)
      model.add(birthdateStmt)
      model.add(genderStmt)
      id += 1
    })*/
  }

  def infoVaccin(person:Resource)={
    val vaccin = generate_vaccin()
    val date = faker.date().past(300, TimeUnit.DAYS)
    val propertyVaccin= ResourceFactory.createProperty("http://swat.cse.lehigh.edu/onto/univ-bench.owl#vaccinated")
    val objectvaccin =ResourceFactory.createResource(person.toString + "#vaccin")
    val vaccinStmt = ResourceFactory.createStatement(person, propertyVaccin, objectvaccin)
    val propertyVaccinName= ResourceFactory.createProperty("http://swat.cse.lehigh.edu/onto/univ-bench.owl#vaccinName")
    val vnameObject = ResourceFactory.createStringLiteral(vaccin.name)
    val vnameStmt = ResourceFactory.createStatement(objectvaccin,propertyVaccinName,vnameObject)
    val propertyVaccinDate= ResourceFactory.createProperty("http://swat.cse.lehigh.edu/onto/univ-bench.owl#vaccinDate")
    val dateObject = ResourceFactory.createStringLiteral(date.toString)
    val dateStmt = ResourceFactory.createStatement(objectvaccin,propertyVaccinDate,dateObject)
    model.add(vaccinStmt)
    model.add(vnameStmt)
    model.add(dateStmt)
  }

  def addInfoVaccinToPersons(vaccinM : Integer=48, vaccinF : Integer=52) ={
    val male = listAllMale()
    val female = listAllFemale()
    val nbMale = (male.size() * vaccinM) / 100
    val nbFemale = (female.size() * vaccinF) / 100
    var count = 0
    male.forEach(prof => {
      if(count < nbMale){
        infoVaccin(prof)
        count += 1
      }
    })
    count = 0
    female.forEach(prof => {
      if(count < nbFemale) {
        infoVaccin(prof)
        count += 1
      }
    })
  }

  def addInfoToProfsJSON()={

  }
  def gen()={
    System.out.println(model.supportsSetOperations())
    System.out.println(model.supportsTransactions())
    val out = new FileOutputStream(new File("/home/2ind2/loic.jouanisson/ProjetVaccin/main/src/resources/out.xml"))
    model.write(out,null)
    out.close
  }

  def toJson() = {
    val mapper = new ObjectMapper()
    mapper.registerModule(DefaultScalaModule)
    val persons = types
    val out = new StringWriter()
    persons.forEach(person => {
      mapper.writeValue(out, generateInfo())
    })
    val json = out.toString()
    System.out.println(json)
  }

  def toJsonVaccin() = {
    val mapper = new ObjectMapper()
    mapper.registerModule(DefaultScalaModule)
    val profs = listAllVaccinated()
    val out = new StringWriter()
    val propertyId = ResourceFactory.createProperty("http://swat.cse.lehigh.edu/onto/univ-bench.owl#id")
    val propertyFname = ResourceFactory.createProperty("http://swat.cse.lehigh.edu/onto/univ-bench.owl#firstName")
    val propertyLname = ResourceFactory.createProperty("http://swat.cse.lehigh.edu/onto/univ-bench.owl#lastName")
    val propertyVaccin= ResourceFactory.createProperty("http://swat.cse.lehigh.edu/onto/univ-bench.owl#vaccinated")
    val propertyVaccinName= ResourceFactory.createProperty("http://swat.cse.lehigh.edu/onto/univ-bench.owl#vaccinName")
    val propertyVaccinDate= ResourceFactory.createProperty("http://swat.cse.lehigh.edu/onto/univ-bench.owl#vaccinDate")
    profs.forEach(prof => {
      val id = model.getProperty(prof,propertyId).getObject.toString
      val fname = model.getProperty(prof,propertyFname).getString
      val lname = model.getProperty(prof,propertyLname).getString
      val vaccin = model.getProperty(prof,propertyVaccin).getResource
      val vname = model.getProperty(vaccin,propertyVaccinName).getString
      val vdate = model.getProperty(vaccin,propertyVaccinDate).getString
      val side_effect = generate_side_effect()
      //System.out.println(id,fname,lname,vname,vdate,side_effect.name,side_effect.SIDER_code)
      mapper.writeValue(out, Person_Vaccinated(vdate,id,fname,lname,vname,side_effect.name,side_effect.SIDER_code))
    })
    val json = out.toString()
    System.out.println(json)
  }
}

object Lubm {
  def apply(dbSource : String) = new Lubm(dbSource)
}

case class Person(fname:String, lname:String,birthdate:String,gender:String)
case class Person_Vaccinated(date:String, id:String,fname:String,lname:String,vname:String,side_effect:String,SIDER_code:String)