package com.gildedrose

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GildedRoseTest extends AnyFlatSpec with Matchers {
  private val brie = "Aged Brie"
  private val sulfuras = "Sulfuras, Hand of Ragnaros"
  private val passes = "Backstage passes to a TAFKAL80ETC concert"

  it should "At the end of each day our system lowers both values for every item" in {
    val app = addItemsAndUpdate(item("foo", 1, 1))
    app.items(0).name should equal("foo")
    app.items(0).sellIn shouldBe 0
    app.items(0).quality shouldBe 0
  }

  it should "Once the sell by date has passed, Quality degrades twice as fast" in {
    val app = addItemsAndUpdate(item("foo", 0, 2))
    app.items(0).sellIn shouldBe -1
    app.items(0).quality shouldBe 0
  }

  it should "The Quality of an item is never negative" in {
    val app = addItemsAndUpdate(item("foo", -2, 0))
    app.items(0).sellIn shouldBe -3
    app.items(0).quality shouldBe 0
  }

  it should "'Aged Brie' actually increases in Quality the older it gets" in {
    val app = addItemsAndUpdate(item(brie, 1, 1))
    app.items(0).sellIn shouldBe 0
    app.items(0).quality shouldBe 2
  }

  it should "'Aged Brie' actually increases in quality by 2 the older it gets, after the sell in" in {
    val app = addItemsAndUpdate(item(brie, -1, 1))
    app.items(0).sellIn shouldBe -2
    app.items(0).quality shouldBe 3
  }

  it should "The Quality of an item is never more than 50" in {
    val app = addItemsAndUpdate(item(brie, 1, 49))
    app.updateQuality()
    app.updateQuality()
    app.items(0).quality shouldBe 50
  }

  it should "'Sulfuras', being a legendary item, never has to be sold or decreases in Quality" in {
    val app = addItemsAndUpdate(item(sulfuras, 10, 11))
    app.items(0).sellIn shouldBe 10
    app.items(0).quality shouldBe 11
  }

  it should "'Backstage passes' increases in Quality as it’s SellIn value approaches" in {
    val app = addItemsAndUpdate(item(passes, 11, 11))
    app.items(0).sellIn shouldBe 10
    app.items(0).quality shouldBe 12
  }

  it should "'Backstage passes' increases in Quality as it’s SellIn value approaches whole flow" in {
    val app = addItemsAndUpdate(item(passes, 10, 11))
    (1 to 9).foreach(_ => app.updateQuality())
    app.items(0).sellIn shouldBe 0
    app.items(0).quality shouldBe 11 + 2 + 2 + 2 + 2 + 2 + 3 + 3 + 3 + 3 + 3
    // ^ 10  9   8   7   6   5   4   3   2   1
  }

  it should "'Backstage passes'  Quality increases by 2 when there are 10 days or less " in {
    val app = addItemsAndUpdate(item(passes, 9, 13))
    app.items(0).sellIn shouldBe 8
    app.items(0).quality shouldBe 15
  }

  it should "'Backstage passes' by 3 when there are 5 days or less" in {
    val app = addItemsAndUpdate(item(passes, 5, 13))
    app.items(0).sellIn shouldBe 4
    app.items(0).quality shouldBe 16
  }

  it should "'Backstage passes' quality drops to 0 after the concert" in {
    val app = addItemsAndUpdate(item(passes, 0, 13))
    app.items(0).sellIn shouldBe -1
    app.items(0).quality shouldBe 0
  }

  it should "'Backstage passes' quality drops to 0 after the concert and remains 0" in {
    val app = addItemsAndUpdate(item(passes, 0, 13))
    app.updateQuality()
    app.updateQuality()
    app.items(0).sellIn shouldBe -3
    app.items(0).quality shouldBe 0
  }

  it should "Process multiple items and keep order" in {
    val app = addItemsAndUpdate(item(brie, 4, 10), item(sulfuras, 6, 10), item("my item", 10, 9))

    app.items(0).name shouldBe brie
    app.items(0).sellIn shouldBe 3
    app.items(0).quality shouldBe 11

    app.items(1).name shouldBe sulfuras
    app.items(1).sellIn shouldBe 6
    app.items(1).quality shouldBe 10

    app.items(2).name shouldBe "my item"
    app.items(2).sellIn shouldBe 9
    app.items(2).quality shouldBe 8
  }

  it should "'Conjured' items degrade in Quality twice as fast as normal items" in {
    val app = addItemsAndUpdate(item("Conjured", 10, 6))

    app.items(0).sellIn shouldBe 9
    app.items(0).quality shouldBe 4
  }

  it should "'Conjured' items degrade in Quality twice but not less then 0" in {
    val app = addItemsAndUpdate(item("Conjured", 10, 5)) // <- test zero branch

    (1 to 5).foreach(_ => app.updateQuality())

    app.items(0).quality shouldBe 0
  }

  private def addItemsAndUpdate(items: Item*): GildedRose = {
    val app = new GildedRose(items.toArray)
    app.updateQuality()
    app
  }

  private def item(name: String, sellIn: Int, quality: Int): Item =
    new Item(name, sellIn, quality)
}
