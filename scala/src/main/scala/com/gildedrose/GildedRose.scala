package com.gildedrose

/** Item type used in Gilded Rose class
  * Use this type instead of Item type because it offers immutability and it uses Quality class in stead of plain integer
  */
sealed trait GItem {
  val name: String
  def sellIn: Int
  def quality: Quality
}
final case class CommonItem(override val name: String, sellIn: Int, quality: Quality) extends GItem
final case class Brie(sellIn: Int, quality: Quality) extends GItem {
  override val name = "Aged Brie"
}
final case class Sulfuras(sellIn: Int, quality: Quality) extends GItem {
  override val name = "Sulfuras, Hand of Ragnaros"
}
final case class Passes(sellIn: Int, quality: Quality) extends GItem {
  override val name = "Backstage passes to a TAFKAL80ETC concert"
}
final case class Conjured(sellIn: Int, quality: Quality) extends GItem {
  override val name = "Conjured"
}

/** Class wrapper over the int quality
  * It encapsulates the quality logic together with the it's companion object so no Quality object will ever be created
  * in an wrong state. "The Quality of an item is never negative" & "The Quality of an item is never more than 50"
  * @param v the quality value
  */
final case class Quality(v: Int) {
  def incrementByOne: Quality = Quality(v + 1)
  def incrementByTwo: Quality = Quality(v + 2)
  def incrementByThree: Quality = Quality(v + 3)

  def decrementByOne: Quality = Quality(v - 1)
  def decrementByTwo: Quality = Quality(v - 2)
}

object Quality {
  def apply(v: Int): Quality =
    if (v <= 0) new Quality(0)
    else if (v >= 50) new Quality(50)
    else new Quality(v)
}

object GItem {

  /** Holdes logic to update quality of an iten based on type
    * @param item to update quality
    * @return
    */
  def updateQuality(item: GItem): GItem = item match {
    case CommonItem(name, sellIn, quality) if sellIn <= 0 =>
      CommonItem(name, sellIn - 1, quality.decrementByTwo)
    case CommonItem(name, sellIn, quality) =>
      CommonItem(name, sellIn - 1, quality.decrementByOne)
    case Brie(sellIn, quality) if sellIn <= 0 => Brie(sellIn - 1, quality.incrementByTwo)
    case Brie(sellIn, quality) => Brie(sellIn - 1, quality.incrementByOne)
    case Passes(sellIn, quality) if sellIn > 10 => Passes(sellIn - 1, quality.incrementByOne)
    case Passes(sellIn, quality) if sellIn <= 10 && sellIn > 5 =>
      Passes(sellIn - 1, quality.incrementByTwo)
    case Passes(sellIn, quality) if sellIn <= 5 && sellIn > 0 =>
      Passes(sellIn - 1, quality.incrementByThree)
    case Passes(sellIn, _) if sellIn <= 0 => Passes(sellIn - 1, Quality(0))
    case Sulfuras(_, _) => item
    case Conjured(sellIn, quality) => Conjured(sellIn - 1, quality.decrementByTwo)
  }

  /** Coverts an item object to an Gilded Rose item
    * @param item to convert
    * @return
    */
  def fromItem(item: Item): GItem = item.name match {
    case "Aged Brie" => Brie(item.sellIn, Quality(item.quality))
    case "Sulfuras, Hand of Ragnaros" => Sulfuras(item.sellIn, Quality(item.quality))
    case "Backstage passes to a TAFKAL80ETC concert" => Passes(item.sellIn, Quality(item.quality))
    case "Conjured" => Conjured(item.sellIn, Quality(item.quality))
    case _ => CommonItem(item.name, item.sellIn, Quality(item.quality))
  }

  /** Converts an Gilded Rose item back to Item
    * @param gItem
    * @return
    */
  def toItem(gItem: GItem): Item = new Item(gItem.name, gItem.sellIn, gItem.quality.v)
}

class GildedRose(var items: Array[Item]) {

  def updateQuality(): Unit = {
    val gItems = items
      .map(GItem.fromItem)
      .map(GItem.updateQuality)
      .map(GItem.toItem)

    items = gItems
  }
}
