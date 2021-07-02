using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Microsoft.ML;
using Microsoft.ML.Data;
using static Microsoft.ML.DataOperationsCatalog;
using Microsoft.ML.Trainers;
using Microsoft.ML.Transforms.Text;
using klbotlib.Modules;
using System.Security.Cryptography;

namespace fucknet
{
    class Program
    {
        static RNGCryptoServiceProvider ro = new();
        static void Main(string[] args)
        {
            //MLContext mlContext = new();
            //TrainTestData splitDataView = LoadData(mlContext);
            //ITransformer model = BuildAndTrainModel(mlContext, splitDataView.TrainSet);
            //Evaluate(mlContext, model, splitDataView.TestSet);
            //生成200初始数据集
            string[] lines = new string[200];
            for (int i = 0; i < lines.Length; i++)
            {
                lines[i] = FuckModule.FuckGenerator(ro);
            }
            File.WriteAllLines("raw_data.txt", lines);
        }

        public static TrainTestData LoadData(MLContext mlContext)
        {
            IDataView dataView = mlContext.Data.LoadFromTextFile<Data>("yelp_labelled.txt", hasHeader: false);
            return mlContext.Data.TrainTestSplit(dataView, testFraction: 0.2);
        }
        public static ITransformer BuildAndTrainModel(MLContext mlContext, IDataView splitTrainSet)
        {
            var estimator = mlContext.Transforms.Text.FeaturizeText(outputColumnName: "Features", inputColumnName: nameof(Data.Text));
            estimator.Append(mlContext.BinaryClassification.Trainers.SdcaLogisticRegression(labelColumnName: "Label", featureColumnName: "Features"));
            Console.WriteLine("=============== Create and Train the Model ===============");
            var model = estimator.Fit(splitTrainSet);
            Console.WriteLine("=============== End of training ===============");
            Console.WriteLine();
            return model;
        }
        public static void Evaluate(MLContext mlContext, ITransformer model, IDataView splitTestSet)
        {
            Console.WriteLine("=============== Evaluating Model accuracy with Test data===============");
            IDataView predictions = model.Transform(splitTestSet);
            CalibratedBinaryClassificationMetrics metrics = mlContext.BinaryClassification.Evaluate(predictions, "Label");
            Console.WriteLine();
            Console.WriteLine("Model quality metrics evaluation");
            Console.WriteLine("--------------------------------");
            Console.WriteLine($"Accuracy: {metrics.Accuracy:P2}");
            Console.WriteLine($"Auc: {metrics.AreaUnderRocCurve:P2}");
            Console.WriteLine($"F1Score: {metrics.F1Score:P2}");
            Console.WriteLine("=============== End of model evaluation ===============");
        }
        private static void UseModelWithSingleItem(MLContext mlContext, ITransformer model)
        {
            PredictionEngine<Data, DataPrediction> predictionFunction = mlContext.Model.CreatePredictionEngine<Data, DataPrediction>(model);
            Data sampleStatement = new Data
            {
                Text = "This was a very bad steak"
            };
            var resultPrediction = predictionFunction.Predict(sampleStatement);
            Console.WriteLine();
            Console.WriteLine("=============== Prediction Test of model with a single sample and test dataset ===============");

            Console.WriteLine();
            Console.WriteLine($"Sentiment: {resultPrediction.Text} | Prediction: {(Convert.ToBoolean(resultPrediction.Prediction) ? "Positive" : "Negative")} | Probability: {resultPrediction.Probability} ");

            Console.WriteLine("=============== End of Predictions ===============");
            Console.WriteLine();
        }
    }

    //数据集中的一条数据
    public class Data
    {
        [LoadColumn(0)]
        public string Text;

        [LoadColumn(1), ColumnName("Label")]
        public bool IsSmooth;   //是否通顺
    }

    public class DataPrediction : Data
    {

        [ColumnName("PredictedLabel")]
        public bool Prediction { get; set; }
        public float Probability { get; set; }
        public float Score { get; set; }
    }
}
