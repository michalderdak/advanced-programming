// Advanced Programming. Andrzej Wasowski. IT University
// To execute this example, run "sbt run" or "sbt test" in the root dir of the project
// Spark needs not to be installed (sbt takes care of it)

import org.apache.spark.ml.feature.Tokenizer
import org.apache.spark.sql.{DataFrame, Dataset, Row}
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.types._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.catalyst.encoders.RowEncoder
import org.apache.spark.ml.linalg.Vectors
import scala.collection.mutable

import org.apache.spark.ml.classification.MultilayerPerceptronClassifier
import org.apache.spark.mllib.util.MLUtils
import org.apache.spark.mllib.evaluation.MulticlassMetrics
import org.apache.spark.ml.evaluation.MulticlassClassificationEvaluator

object Main {

	type Embedding       = (String, List[Double])
	type ParsedReview    = (Integer, String, Double)

	org.apache.log4j.Logger getLogger "org"  setLevel (org.apache.log4j.Level.WARN)
	org.apache.log4j.Logger getLogger "akka" setLevel (org.apache.log4j.Level.WARN)
	val spark =  SparkSession.builder
		.appName ("Sentiment")
		.master  ("local[9]")
		.getOrCreate

  	import spark.implicits._

	val reviewSchema = StructType(Array(
			StructField ("reviewText", StringType, nullable=false),
			StructField ("overall",    DoubleType, nullable=false),
			StructField ("summary",    StringType, nullable=false)))

	// Read file and merge the text abd summary into a single text column

	def loadReviews (path: String): Dataset[ParsedReview] = {
		spark
			.read
			.schema (reviewSchema)
			.json (path)
			.rdd
			.zipWithUniqueId
			.map[(Integer,String,Double)] { case (row,id) => (id.toInt, s"${row getString 2} ${row getString 0}", row getDouble 1) }
			.toDS
			.withColumnRenamed ("_1", "id" )
			.withColumnRenamed ("_2", "text")
			.withColumnRenamed ("_3", "overall")
			.as[ParsedReview]
	}

  // Load the GLoVe embeddings file

	def loadGlove (path: String): Dataset[Embedding] =  {
			spark
				.read
				.text (path)
		.map  { _ getString 0 split " " }
		.map  (r => (r.head, r.tail.toList.map (_.toDouble))) // yuck!
				.withColumnRenamed ("_1", "word" )
				.withColumnRenamed ("_2", "vec")
				.as[Embedding]

	}

	def tokenize(reviews: Dataset[ParsedReview]): Dataset[Row] = {
        val tokenizer = new Tokenizer()
        tokenizer.setInputCol("text")
        tokenizer.setOutputCol("words")

        val schema = StructType(
            Array(
                StructField ("id", IntegerType, nullable = false),
                StructField ("words", DataTypes.createArrayType(StringType), nullable = false),
                StructField ("label", DoubleType, nullable = false)
            )
        )
 
        val encoder = RowEncoder(schema)
 
        tokenizer.transform(reviews)
        .select($"id", $"words", $"overall")
            .map {
                case Row(id: Int, words: Seq[String], label: Double) =>
                    Row(id, words,
                        if (label < 3) 0.0
                        else if (label == 3) 1.0
                        else 2.0
                    )
                } (encoder)
    }
 
  	def flattenTokens(reviews: Dataset[Row]): Dataset[ParsedReview] = {
		reviews.flatMap {
			case Row(id: Int, words: Seq[String], label: Double) => {
				words.map(x => (id, x.toLowerCase(), label))
			}
		}.as[ParsedReview]
  	}

  	def vectorize(reviews: Dataset[ParsedReview], glove: Dataset[Embedding]): Dataset[Row] = {
		val schema = StructType (
			Array(
				StructField("id", IntegerType, nullable = false),
				StructField("attributes", DataTypes.createArrayType(DoubleType), nullable = false)
			)
		)
		val encoder = RowEncoder(schema)

		val aggregated = reviews.join(glove, reviews("_2") === glove("word"))	
        val numUnique = aggregated.groupByKey(r => r.getAs[Int](0)).count

		val vectorSum = aggregated
			.drop("_2", "word", "_3", "label") 
			.groupByKey(r => r.getAs[Int]("_1"))
			.reduceGroups((a1, a2) => Row(a1.getInt(0), (a1.getSeq(1), a2.getSeq(1))
			.zipped
			.map((a1: Double, a2: Double) => a1 + a2)))
			.map {
				case (_, r: Row) => {
					Row(r.getInt(0), r.getAs[mutable.WrappedArray[Double]](1).flatMap((x: Double) => Seq(x)))
				}
			} (encoder)
	
		val vecCount = vectorSum.join(numUnique, vectorSum("id") === numUnique("value")).drop("value")
	
		vecCount.map {
			case Row(id: Int, attr: Seq[Double], num: Long) => {
				Row(id, attr.map((x: Double) => x / num))
			}
		} (encoder)
	}

	def finalize(vectorizedReviews: Dataset[Row], tokenizedReviews: Dataset[Row]): DataFrame = {
		vectorizedReviews
			.join(tokenizedReviews, Seq("id"))
			.drop("words", "id")
			.as[(Array[Double], Double)]
			.map { 
				case (attributes: Array[Double], target: Double) => {
					(Vectors.dense(attributes), target)
				}
			}
			.withColumnRenamed ("_1", "features")
			.withColumnRenamed ("_2", "label")
	}

	def train(df: DataFrame): Unit = {
		val splits = df.randomSplit(Array(0.8, 0.2), seed = 1234L)
		val train = splits(0)
		val test = splits(1)
		
		val layers = Array[Int](50, 100, 3)

		val trainer = new MultilayerPerceptronClassifier()
			.setLayers(layers)
			.setBlockSize(128)
			.setSeed(1234L)
			.setMaxIter(100)

		val model = trainer.fit(train)

		val result = model.transform(test)
		val predictionAndLabels = result.select("prediction", "label")
		val evaluator = new MulticlassClassificationEvaluator()
			.setMetricName("accuracy")

		println(s"Test set accuracy = ${evaluator.evaluate(predictionAndLabels)}")
	}

  	def main(args: Array[String]) = {

		val glove  = loadGlove("./glove.6B.50d.txt") // FIXME
		val rawReviews = loadReviews("./reviews.json") // FIXME 
		
		val tokenizedReviews = tokenize(rawReviews)
		tokenizedReviews.show
		val flattenReviews = flattenTokens(tokenizedReviews)
		flattenReviews.show
		val vectorizedReviews = vectorize(flattenReviews, glove)
		vectorizedReviews.show
		val reviews = finalize(vectorizedReviews, tokenizedReviews)
		reviews.show

		train(reviews)

		spark.stop	
	}

}
