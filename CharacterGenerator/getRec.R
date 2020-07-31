getRec <- function(x){
  if(names(x[1]) == "Strength" | names(x[2]) == "Strength"){
    if(names(x[1]) == "Dexterity" | names(x[2]) == "Dexterity"){
      if(names(x[3]) == "Constitution"){
        paste("Barbarian, Fighter. 
              Note that this is based in part on your third highest score, " , names(x[3]), ".")
      }else if(names(x[3]) == "Intelligence"){
        paste("Fighter (Edritch Knight), Rogue (Arcane Trickster), Wizard. 
              Note that this is based in part on your third highest score, " , names(x[3]), ".")
      }else if(names(x[3]) == "Wisdom"){
        paste("Cleric, Ranger, Monk.  
              Note that this is based in part on your third highest score, " , names(x[3]), ".")
      }else if(names(x[3]) == "Charisma"){
        paste("Paladin, Bard, Rogue.
              Note that this is based in part on your third highest score, " , names(x[3]), ".")
      }
    }else if(names(x[1]) == "Constitution" | names(x[2]) == "Constitution"){
      paste("Barbarian, Fighter (melee weapons).")
    }else if(names(x[1]) == "Intelligence" | names(x[2]) == "Intelligence"){
      paste("Fighter (Edritch Knight).")
    }else if(names(x[1]) == "Wisdom" | names(x[2]) == "Wisdom"){
      paste("Cleric, Ranger (two handed weapons).")
    }else if(names(x[1]) == "Charisma" | names(x[2]) == "Charisma"){
      paste("Paladin.")
    }
  }else if(names(x[1]) == "Dexterity" | names(x[2]) == "Dexterity"){
    if(names(x[1]) == "Constitution" | names(x[2]) == "Constitution"){
      paste("Fighter (archer).")
    }else if(names(x[1]) == "Intelligence" | names(x[2]) == "Intelligence"){
      paste("Fighter (Edritch Knight), Rogue (Arcane Trickster), Wizard.")
    }else if(names(x[1]) == "Wisdom" | names(x[2]) == "Wisdom"){
      paste("Monk, Ranger.")
    }else if(names(x[1]) == "Charisma" | names(x[2]) == "Charisma"){
      paste("Bard, Rogue.")
    }
  }else if(names(x[1]) == "Constitution" | names(x[2]) == "Constitution"){
    if(names(x[1]) == "Intelligence" | names(x[2]) == "Intelligence"){
      paste("Wizard.")
    }else if(names(x[1]) == "Wisdom" | names(x[2]) == "Wisdom"){
      paste("Cleric, Druid.")
    }else if(names(x[1]) == "Charisma" | names(x[2]) == "Charisma"){
      paste("Sorcerer, Warlock.")
    }
  }else if(names(x[1]) == "Intelligence" | names(x[2]) == "Intelligence"){
    if(names(x[1]) == "Wisdom" | names(x[2]) == "Wisdom"){
      if(names(x[3]) == "Constitution"){
        paste("Wizard, Cleric, Druid. 
              Note that this is based in part on your third highest score, " , names(x[3]), ".")
      }else if(names(x[3]) == "Dexterity"){
        paste("Fighter (Edritch Knight), Rogue (Arcane Trickster), Wizard, Monk, Ranger. 
              Note that this is based in part on your third highest score, " , names(x[3]), ".")
      }else if(names(x[3]) == "Strength"){
        paste("Fighter (Edritch Knight), Cleric, Ranger (two handed weapons). 
              Note that this is based in part on your third highest score, " , names(x[3]), ".")
      }else if(names(x[3]) == "Charisma"){
        paste("Wizard (School of Enchantment).
              Note that this is based in part on your third highest score, " , names(x[3]), ".")
      }
    }else if(names(x[1]) == "Charisma" | names(x[2]) == "Charisma"){
      paste("Wizard (School of Enchantment)") 
    }
  }else{
    #wisdom & charisma
    if(names(x[3]) == "Constitution"){
      paste("Cleric, Druid, Sorcerer, Warlock.
        Note that this is based in part on your third highest score, " , names(x[3]), ".")
    }else if(names(x[3]) == "Intelligence"){
      paste("Wizard (School of Enchantment).
        Note that this is based in part on your third highest score, " , names(x[3]), ".")
    }else if(names(x[3]) == "Dexterity"){
      paste("Monk, Ranger, Bard, Rogue.
        Note that this is based in part on your third highest score, " , names(x[3]), ".")
    }else if(names(x[3]) == "Strength"){
      paste("Cleric, Ranger (two handed weapons), Paladin. 
        Note that this is based in part on your third highest score, " , names(x[3]), ".")
    }
  }
}