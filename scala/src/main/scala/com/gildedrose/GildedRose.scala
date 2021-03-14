package com.gildedrose

sealed trait GItem {
  val name: String
  def sellIn: Int
  def quality: Int
}
final case class CommonItem(override val name: String, sellIn: Int, quality: Int) extends GItem
final case class Brie(sellIn: Int, quality: Int) extends GItem { override val name = "Aged Brie" }
final case class Sulfuras(sellIn: Int, quality: Int) extends GItem {
  override val name = "Sulfuras, Hand of Ragnaros"
}
final case class Passes(sellIn: Int, quality: Int) extends GItem {
  override val name = "Backstage passes to a TAFKAL80ETC concert"
}
final case class Conjured(sellIn: Int, quality: Int) extends GItem {
  override val name = "Conjured"
}

object GItem {
  private def qualityLimits(quality: Int): Int =
    if (quality < 0) 0
    else if (quality > 50) 50
    else quality
  def updateQuality(item: GItem): GItem = item match {
    case CommonItem(name, sellIn, quality) if sellIn <= 0 =>
      CommonItem(name, sellIn - 1, qualityLimits(quality - 2))
    case CommonItem(name, sellIn, quality) =>
      CommonItem(name, sellIn - 1, qualityLimits(quality - 1))
    case Brie(sellIn, quality) if sellIn <= 0 => Brie(sellIn - 1, qualityLimits(quality + 2))
    case Brie(sellIn, quality) => Brie(sellIn - 1, qualityLimits(quality + 1))
    case Passes(sellIn, quality) if sellIn > 10 => Passes(sellIn - 1, qualityLimits(quality + 1))
    case Passes(sellIn, quality) if sellIn <= 10 && sellIn > 5 =>
      Passes(sellIn - 1, qualityLimits(quality + 2))
    case Passes(sellIn, quality) if sellIn <= 5 && sellIn > 0 =>
      Passes(sellIn - 1, qualityLimits(quality + 3))
    case Passes(sellIn, _) if sellIn <= 0 => Passes(sellIn - 1, 0)
    case Sulfuras(sellIn, quality) => Sulfuras(sellIn, qualityLimits(quality))
    case Conjured(sellIn, quality) => Conjured(sellIn - 1, qualityLimits(quality - 2))
  }
  def fromItem(item: Item): GItem = item.name match {
    case "Aged Brie" => Brie(item.sellIn, item.quality)
    case "Sulfuras, Hand of Ragnaros" => Sulfuras(item.sellIn, item.quality)
    case "Backstage passes to a TAFKAL80ETC concert" => Passes(item.sellIn, item.quality)
    case "Conjured" => Conjured(item.sellIn, item.quality)
    case _ => CommonItem(item.name, item.sellIn, item.quality)
  }
  def toItem(gItem: GItem): Item = new Item(gItem.name, gItem.sellIn, gItem.quality)
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
