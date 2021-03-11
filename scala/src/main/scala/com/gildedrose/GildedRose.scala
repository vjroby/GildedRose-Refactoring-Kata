package com.gildedrose


sealed trait GItem {
  val name: String
  def sellIn: Int
  def quality: Int
}
case class MyItem(override  val name: String, sellIn: Int, quality: Int) extends GItem
case class Brie(sellIn: Int, quality: Int) extends GItem{ override val name = "Aged Brie"}
case class Sulfuras(sellIn: Int, quality: Int) extends  GItem{ override val name = "Sulfuras, Hand of Ragnaros"}
case class Passes(sellIn: Int, quality: Int) extends  GItem{ override val name = "Backstage passes to a TAFKAL80ETC concert"}
case class Conjured(sellIn: Int, quality: Int) extends  GItem{ override val name = "Conjured"}

object GItem{
  def updateQuality(item:GItem):GItem = item match {
    case MyItem(name,sellIn,quality) if sellIn <= 0 =>  MyItem(name,sellIn -1, quality -2 )
    case MyItem(name,sellIn,quality)  =>  MyItem(name, sellIn -1, quality -1)
    case Brie(sellIn,quality) if sellIn <= 0 =>  Brie(sellIn -1, quality +2 )
    case Brie(sellIn,quality)  =>  Brie( sellIn -1, quality +1)
    case Passes(sellIn,quality) if sellIn >10  =>  Passes( sellIn -1, quality + 1)
    case Passes(sellIn,quality) if sellIn <=10 && sellIn >5 =>  Passes( sellIn -1, quality + 2)
    case Passes(sellIn,quality) if sellIn <=5 && sellIn >0 =>  Passes( sellIn -1, quality + 3)
    case Passes(sellIn,quality) if sellIn <=0  =>  Passes( sellIn -1, 0)
    case Sulfuras(sellIn,quality) =>  Sulfuras(sellIn, quality)
    case Conjured(sellIn,quality) =>  Conjured(sellIn, quality -2)
  }
  def fromItem(item: Item):GItem = item.name match {
    case "Aged Brie" => Brie(item.sellIn,item.quality)
    case "Sulfuras, Hand of Ragnaros" => Sulfuras(item.sellIn,item.quality)
    case "Backstage passes to a TAFKAL80ETC concert" => Passes(item.sellIn,item.quality)
    case "Conjured" => Conjured(item.sellIn,item.quality)
    case _ => MyItem(item.name,item.sellIn,item.quality)
  }
  def toItem(gItem: GItem):Item = new Item(gItem.name, gItem.sellIn, gItem.quality)
}

 class GildedRose(var items: Array[Item]) {

   def updateQuality(): Unit = {
     val gItems = items.map(GItem.fromItem)
       .map(GItem.updateQuality)
       .map(GItem.toItem)

     items = gItems
   }

   def updateQualityOld() {
    for (i <- 0 until items.length) {
      if (!items(i).name.equals("Aged Brie")
        && !items(i).name.equals("Backstage passes to a TAFKAL80ETC concert")) {
        if (items(i).quality > 0) {
          if (!items(i).name.equals("Sulfuras, Hand of Ragnaros")) {
            if (!items(i).name.equals("Conjured")){
              items(i).quality = items(i).quality - 1
            }else{
              items(i).quality = if ( items(i).quality <= 1){
                0
              }else{
                items(i).quality - 2
              }
            }
          }
        }
      } else {
        if (items(i).quality < 50) {
          items(i).quality = items(i).quality + 1

          if (items(i).name.equals("Backstage passes to a TAFKAL80ETC concert")) {
            if (items(i).sellIn < 11) {
              if (items(i).quality < 50) {
                items(i).quality = items(i).quality + 1
              }
            }

            if (items(i).sellIn < 6) {
              if (items(i).quality < 50) {
                items(i).quality = items(i).quality + 1
              }
            }
          }
        }
      }

      if (!items(i).name.equals("Sulfuras, Hand of Ragnaros")) {
        items(i).sellIn = items(i).sellIn - 1
      }

      if (items(i).sellIn < 0) {
        if (!items(i).name.equals("Aged Brie")) {
          if (!items(i).name.equals("Backstage passes to a TAFKAL80ETC concert")) {
            if (items(i).quality > 0) {
              if (!items(i).name.equals("Sulfuras, Hand of Ragnaros")) {
                items(i).quality = items(i).quality - 1
              }
            }
          } else {
            items(i).quality = items(i).quality - items(i).quality
          }
        } else {
          if (items(i).quality < 50) {
            items(i).quality = items(i).quality + 1
          }
        }
      }
    }
  }
}
